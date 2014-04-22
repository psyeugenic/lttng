%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    lttng_service.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-04-15
%%
%% Port (5345) on localhost (127.0.0.1).

-module(lttng_service).

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

handle_info({tcp,Port,<<Sz:64/big,Cmd:32/big,Vsn:32/big>>}, #state{port=Port}=S0) ->
    S1 = receive_command(Sz,Cmd,Vsn,S0),
    inet:setopts(Port,[{active,once}]),
    {noreply, S1};
handle_info({tcp,Port,B}, #state{port=Port}=S) ->
    io:format("B ~p~n", [B]),
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

receive_command(0,Cmd,_Vsn,S) -> handle_command(Cmd,[],S);
receive_command(Sz,Cmd,_Vsn,#state{ port = P} = S) ->
    {ok,B} = gen_tcp:recv(P,Sz),
    handle_command(Cmd,B,S).

handle_command(?lttng_reg_done,_Payload,S0) ->
    error_logger:info_msg("LTTNG-JUL registration completed.\n"),
    S0;
handle_command(?lttng_cmd_list,_Payload,#state{ port = Port } = S0) ->
    error_logger:info_msg("LTTNG-JUL command list requested.\n"),
    %% data
    %% buf.putInt(code.getCode());
    %% buf.putInt(data_size);
    %% buf.putInt(nb_logger);
    Ls = dummy_loggers(),
    {Sz,N} = lists:foldl(fun
	    (B,{Szi,Ni}) -> {Szi+byte_size(B)+1,Ni+1}
	end, {0,0}, Ls),
    Names = << <<Name/binary,0>> || Name <- Ls >>,
    Data = <<?lttng_cmd_success:32/big,Sz:32/big,N:32/big,Names/binary>>,
    ok = gen_tcp:send(Port, Data),
    S0;
handle_command(Cmd,Data,S0) ->
    io:format("cmd: ~p ~p~n", [Cmd,Data]),
    S0.

dummy_loggers() -> [
	<<"myerlanglog">>,
	<<"an erlang logger 1">>,
	<<"another erlang logger">>,
	<<"a second erlang logger">>
    ].
