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


-record(handler, {
	active = false :: boolean(),
	patterns = []  :: [{mfa(),term()}]
    }).

-record(state, {
	handlers = gb_trees:empty(),
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
    %% ensure trace points loaded
    {module,lttng} = code:ensure_loaded(lttng),

    %% add handlers from environment
    S = lists:foldl(fun
	    ({{App,Cmd},Ps},Si) ->
		handler_insert(atom_to_binary(App,utf8),Cmd,Ps,Si)
	end, #state{}, application:get_env(lttng,handlers,[])),

    PortNo = application:get_env(lttng,port,5345),
    IpAddr = application:get_env(lttng,ip,{127,0,0,1}),

    %% handshake lttng jul
    {ok,Port} = gen_tcp:connect(IpAddr,PortNo,[{active,once},binary]),
    Pid = list_to_integer(os:getpid()),
    ok = gen_tcp:send(Port, <<Pid:32/big>>),
    {ok, S#state{port=Port}}.

handle_call({add_handler,App,Cmd,Ps}, _From, S) ->
    case valid_handler_patterns(Ps) of
	false -> {reply, error, S};
	true ->
	    supervisor_state_insert({App,Cmd},Ps),
	    {reply, ok, handler_insert(atom_to_binary(App,utf8),Cmd,Ps,S) }
    end;

handle_call({delete_handler,App,Cmd}, _From, S) ->
    supervisor_state_delete({App,Cmd}),
    {reply, ok, handler_delete(atom_to_binary(App,utf8),Cmd,S) };


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
    Ls = handler_list_names(S0),
    Names = << <<Name/binary,0>> || Name <- Ls >>,
    Sz = byte_size(Names),
    N = length(Ls),
    {<<?lttng_cmd_success:32/big,Sz:32/big,N:32/big,Names/binary>>, S0};

handle_command(?lttng_cmd_enable,<<Level:32/signed-little,Type:32/signed-little,Str/binary>>,S0) ->
    Name = chomp(Str),
    case enable_trace(log_level(Level),Type,Name,S0) of
	{true,S1}  ->
	    ?info("LTTNG-JUL enable-event\n  log-level: ~w\n"
		"  log-type : ~w~n  string   : ~ts~n",[log_level(Level),Type,Name]),
	    {<<?lttng_cmd_success:32/big>>,S1};
	{false,S1} ->
	    ?error("LTTNG-JUL enable-event failed: ~ts~n", [Name]),
	    {<<?lttng_cmd_invalid:32/big>>,S1}
    end;

handle_command(?lttng_cmd_disable,Str,S0) ->
    Name = chomp(Str),
    case disable_trace(Name,S0) of
	{true,S1}  ->
	    ?info("LTTNG-JUL disable-event: ~ts~n", [Name]),
	    {<<?lttng_cmd_success:32/big>>,S1};
	{false,S1} ->
	    ?error("LTTNG-JUL disable-event failed: ~ts~n", [Name]),
	    {<<?lttng_cmd_invalid:32/big>>,S1}
    end;

handle_command(Cmd,Data,S0) ->
    ?error("Invalid cmd: ~p, data: ~p~n", [Cmd,Data]),
    {<<?lttng_cmd_invalid:32/big>>,S0}.

enable_trace(_Level,_Type,Search,S0) ->
    try
	case handler_search_patterns(Search,S0) of
	    {true,Ps} ->
		S1 = set_trace(Ps,S0),
		S2 = handler_search_set_active(Search,true,S1),
		{true, S2};
	    false ->
		{false,S0}
	end
    catch
	error:_Reason ->
	    {false,S0}
    end.

disable_trace(Search,S0) ->
    try
	case handler_search_patterns(Search,S0) of
	    {true,Ps} ->
		ok = set_trace_patterns([{Mfa,false}||{Mfa,_}<-Ps]),
		S1 = handler_search_set_active(Search,false,S0),
		{true,S1};
	    false ->
		{false,S0}
	end
    catch
	error:_Reason ->
	    {false,S0}
    end.


set_trace(Patterns,#state{ tracer=undefined }=S) ->
    set_trace(Patterns,S#state{ tracer=start_tracer() });
set_trace(Patterns,#state{ tracer=T }=S) ->
    _  = erlang:trace(all,true,[call,{tracer,T}]),
    ok = set_trace_patterns(Patterns),
    S.

set_trace_patterns([]) -> ok;
set_trace_patterns([{Mfa,Ms}|Ps]) ->
    _ = erlang:trace_pattern(Mfa,Ms,[local]),
    set_trace_patterns(Ps).

handler_search_patterns(Search,S) ->
    [App,Cmd] = binary:split(Search,<<":">>,[global]),
    handler_get_patterns(App,Cmd,S).
	    

handler_search_set_active(Search,Bool,S) ->
    [App,Cmd] = binary:split(Search,<<":">>,[global]),
    handler_set_active(App,Cmd,Bool,S).

start_tracer() ->
    spawn_link(fun() -> tracer_loop() end).

tracer_loop() ->
    receive
	{trace,Pid,Type,Msg} ->
	    lttng:erlang_trace(Pid,Type,Msg),
	    tracer_loop();
	_ ->
	    tracer_loop()
    end.

handler_insert(App,Cmd,Ps,#state{ handlers=Hs }=S) ->
    S#state{
	handlers=gb_trees:enter({App,Cmd},#handler{patterns=Ps},Hs)
    }.

handler_delete(App,Cmd,#state{ handlers=Hs }=S) ->
    case gb_trees:is_defined({App,Cmd},Hs) of
	false -> S;
	true ->
	    S#state{
		handlers=gb_trees:delete({App,Cmd},Hs)
	    }
    end.

handler_get_patterns(App,Cmd,#state{ handlers=Hs }) ->
    case gb_trees:lookup({App,Cmd},Hs) of
	none -> false;
	{value,#handler{patterns=Ps}} ->
	    {true,Ps}
    end.

handler_list_names(#state{ handlers=Hs }) ->
    [ begin
		Enabled = enabled_string(B),
		<<App/binary,":",Cmd/binary, " ", Enabled/binary>>
	end || {{App,Cmd},#handler{active=B}}<-gb_trees:to_list(Hs)].

enabled_string(true) -> <<"[enabled]">>;
enabled_string(false) -> <<"[disabled]">>.

handler_set_active(App,Cmd,Bool,#state{handlers=Hs}=S) ->
    H = gb_trees:get({App,Cmd},Hs),
    S#state{handlers=gb_trees:enter({App,Cmd},H#handler{active=Bool},Hs)}.

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


%% update supervisor state

supervisor_state_insert(Key,Ps) ->
    Hs0 = application:get_env(lttng,handlers,[]),
    Hs1 = proplists:delete(Key,Hs0),
    application:set_env(lttng,handlers,[{Key,Ps}|Hs1]).

supervisor_state_delete(Key) ->
    Hs0 = application:get_env(lttng,handlers,[]),
    Hs1 = proplists:delete(Key,Hs0),
    application:set_env(lttng,handlers,Hs1).


%% trace patterns

valid_handler_patterns([P]) ->
    valid_handler_pattern(P);
valid_handler_patterns([P|Ps]) ->
    case valid_handler_pattern(P) of
	true -> valid_handler_patterns(Ps);
	false -> false
    end.

valid_handler_pattern({Mfa,_}) ->
    case Mfa of
	{M,F,A} when is_atom(M),is_atom(F),is_integer(A) -> true;
	_ -> false
    end;
valid_handler_pattern(_) -> false.


chomp(<<>>) -> <<>>;
chomp(<<0,_/binary>>) -> <<>>;
chomp(<<V,B0/binary>>) ->
    B = chomp(B0),
    <<V,B/binary>>.
