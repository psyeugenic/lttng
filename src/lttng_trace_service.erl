%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    lttng_service.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-04-15
%%
%% Port (5345) on localhost (127.0.0.1).

-module(lttng_trace_service).

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([clean/1]).
-define(SERVER, ?MODULE).

-define(lttng_cmd_list, 1).
-define(lttng_cmd_enable, 2).
-define(lttng_cmd_disable, 3).
-define(lttng_reg_done, 4).

-define(lttng_cmd_success, 1).
-define(lttng_cmd_invalid, 2).
-define(lttng_unknown_logger_name, 3).

-define(info(F,Ts), error_logger:info_msg(io_lib:format(F,Ts))).
-define(error(F,Ts), error_logger:error_msg(io_lib:format(F,Ts))).
-record(state, {
	port,
	tracer
    }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok,Port} = gen_tcp:connect({127,0,0,1},5345,[{active,once},binary]),
    Pid = list_to_integer(os:getpid()),
    ok = gen_tcp:send(Port, <<Pid:32/big>>),
    {ok, #state{port=Port}}.

handle_call(_Request, _From, S) ->
    Reply = ok,
    {reply, Reply, S}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp,Port,<<Sz:64/big,Cmd:32/big,Vsn:32/big,Data/binary>>}, #state{port=Port}=S0) ->
    S1 = receive_command(Sz - byte_size(Data),Cmd,Vsn,Data,S0),
    inet:setopts(Port,[{active,once}]),
    {noreply, S1};
handle_info({tcp,Port,B}, #state{port=Port}=S) ->
    ?error("bad message received: ~p\n",[B]),
    inet:setopts(Port,[{active,once}]),
    {noreply, S}.


terminate(_Reason, #state{port=P}) when is_port(P) ->
    ok = gen_tcp:close(P);
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

receive_command(0,Cmd,_Vsn,Data,S) -> process_command(Cmd,Data,S);
receive_command(Sz,Cmd,_Vsn,Data,#state{ port = P} = S) ->
    {ok,B} = gen_tcp:recv(P,Sz),
    process_command(Cmd,<<Data/binary,B/binary>>,S).

process_command(Cmd,Data,S) ->
    case handle_command(Cmd,Data,#state{ port = P} = S) of
	{<<>>, S1} -> S1;
	{Ret, S1} ->
	    ok = gen_tcp:send(P, Ret),
	    S1
    end.

handle_command(?lttng_reg_done,_Data,S0) ->
    ?info("LTTNG-JUL registration completed\n",[]),
    {<<>>,S0};
handle_command(?lttng_cmd_list,_Data,S0) ->
    ?info("LTTNG-JUL command list requested\n",[]),
    Ls = dummy_loggers(),
    {Sz,N} = lists:foldl(fun
	    (B,{Szi,Ni}) -> {Szi+byte_size(B)+1,Ni+1}
	end, {0,0}, Ls),
    Names = << <<Name/binary,0>> || Name <- Ls >>,
    {<<?lttng_cmd_success:32/big,Sz:32/big,N:32/big,Names/binary>>, S0};

handle_command(?lttng_cmd_enable,<<Level:32/signed-little,Type:32/signed-little,Str/binary>>,S0) ->
    Search = chomp(Str),
    ?info("LTTNG-JUL enable command\n  log-level: ~w\n"
	  "   log-type: ~w~n     string: ~ts~n",[log_level(Level),Type,Search]),
    case enable_trace(log_level(Level),Type,Search,S0) of
	{true,S1}  -> {<<?lttng_cmd_success:32/big>>,S1};
	{false,S1} -> {<<?lttng_cmd_invalid:32/big>>,S1}
    end;

handle_command(?lttng_cmd_disable,Str,S0) ->
    Name = chomp(Str),
    ?info("LTTNG-JUL disable command\n"
	  "       name: ~ts~n",[Name]),
    case disable_trace(Name,S0) of
	{true,S1}  -> {<<?lttng_cmd_success:32/big>>,S1};
	{false,S1} -> {<<?lttng_cmd_invalid:32/big>>,S1}
    end;

handle_command(Cmd,Data,S0) ->
    ?error("Invalid cmd: ~p, data: ~p~n", [Cmd,Data]),
    {<<?lttng_cmd_invalid:32/big>>,S0}.

%% dummies
dummy_loggers() -> [
	<<"myerlanglog">>,
	<<"an erlang logger 1">>,
	<<"another erlang logger">>,
	<<"a second erlang logger">>
    ].

enable_trace(_Level,_Type,Search,S) ->
    try
	io:format("search ~p~n", [Search]),
	{Proc,MFA} = parse_search(Search),
	S1 = set_trace(Proc,MFA,S),
	{true,S}
    catch
	C:Reason ->
	    ?error("Invalid trace search: ~p, reason: ~p~n~p~n", [C,Reason,erlang:get_stacktrace()]),
	    {false,S}
    end.

disable_trace(_Name,S) ->
    {true,S}.

parse_search(<<"*">>) -> {all,{'_','_','_'}};
parse_search(Search) ->
    {Proc,Rest} = case binary:split(Search,<<"|">>, [global]) of
	[R] -> {all,R};
	[T,R] ->
	    {bin_to_atom(clean(T)),R}
    end,
    MFA = case binary:split(Rest,[<<":">>,<<"/">>], [global]) of
	[M,F,A] ->
	    {bin_to_msatom(clean(M)),bin_to_msatom(clean(F)),bin_to_msint(clean(A))}
    end,
    {Proc,MFA}.


set_trace(Spec,MFA,#state{ tracer=undefined }=S) ->
    set_trace(Spec,MFA,S#state{ tracer=start_tracer() });
set_trace(Spec,MFA,#state{ tracer=T }=S) ->
    _ = erlang:trace(Spec,true,[call,{tracer,T}]),
    _ = erlang:trace_pattern(MFA,true,[local]),
    S.


start_tracer() ->
    spawn_link(fun() -> tracer_loop() end).

tracer_loop() ->
    receive
	Msg ->
	    lttng:erlang_trace(iolist_to_binary(io_lib:format("~p~n", [Msg]))),
	    tracer_loop()
    end.


%% aux

-define(lttng_jul_severe , 1000).
-define(lttng_jul_warning , 900).
-define(lttng_jul_info ,    800).
-define(lttng_jul_config,   700).
-define(lttng_jul_fine ,    500).
-define(lttng_jul_finer,    400).
-define(lttng_jul_finest ,  300).
-define(lttng_jul_all, -2147483648). %% int32_min
-define(lttng_jul_off,  2147483647). %% int32_max

log_level(Val) ->
    case Val of
	?lttng_jul_off     -> off;
	?lttng_jul_severe  -> severe;
	?lttng_jul_warning -> warning;
	?lttng_jul_info    -> info;
	?lttng_jul_config  -> config;
	?lttng_jul_fine    -> fine;
	?lttng_jul_finer   -> finer;
	?lttng_jul_finest  -> finest;
	?lttng_jul_all     -> all
    end.

bin_to_atom(B) ->
    erlang:binary_to_existing_atom(B,utf8).


bin_to_msatom(<<"*">>) -> '_';
bin_to_msatom(<<"_">>) -> '_';
bin_to_msatom(B) -> bin_to_atom(B).

bin_to_msint(<<"*">>) -> '_';
bin_to_msint(<<"_">>) -> '_';
bin_to_msint(B) ->
    erlang:binary_to_integer(B).



chomp(<<>>) -> <<>>;
chomp(<<0,_/binary>>) -> <<>>;
chomp(<<V,B0/binary>>) ->
    B = chomp(B0),
    <<V,B/binary>>.

%% no whitespace
clean(S) -> clean_lead(clean_trail(S)).

clean_lead(<<>>) -> <<>>;
clean_lead(<<V,R/binary>>=B) ->
    case is_whitespace(V) of
	true  -> clean_lead(R);
	false -> B
    end.

clean_trail(<<>>) -> <<>>;
clean_trail(B) ->
    clean_trail(B,byte_size(B)-1).

clean_trail(B,N) ->
    <<R:N/binary,V>> = B,
    case is_whitespace(V) of
	true  -> clean_trail(R);
	false -> B
    end.

is_whitespace(V) ->
    case V of
	09 -> true;
	10 -> true;
	13 -> true;
	32 -> true;
	_  -> false
    end.

