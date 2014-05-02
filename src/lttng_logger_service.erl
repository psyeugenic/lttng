%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    lttng_service.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-04-15
%%
%% Port (5345) on localhost (127.0.0.1).

-module(lttng_logger_service).

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

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
	port
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
    case dummy_enable_loggers(log_level(Level),Type,Search) of
	true  -> {<<?lttng_cmd_success:32/big>>,S0};
	false -> {<<?lttng_cmd_invalid:32/big>>,S0}
    end;

handle_command(?lttng_cmd_disable,Str,S0) ->
    Name = chomp(Str),
    ?info("LTTNG-JUL disable command\n"
	  "       name: ~ts~n",[Name]),
    case dummy_disable_loggers(Name) of
	true  -> {<<?lttng_cmd_success:32/big>>,S0};
	false -> {<<?lttng_cmd_invalid:32/big>>,S0}
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

dummy_enable_loggers(Level,Type,Search) ->
    ?info("LTTNG-JUL enable command\n  log-level: ~w\n"
	  "   log-type: ~w~n     string: ~ts~n",[Level,Type,Search]),
    true.

dummy_disable_loggers(_Name) ->
    true.

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

chomp(<<>>) -> <<>>;
chomp(<<0,_/binary>>) -> <<>>;
chomp(<<V,B0/binary>>) ->
    B = chomp(B0),
    <<V,B/binary>>.

