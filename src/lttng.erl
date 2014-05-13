-module(lttng).

-export([user_tracepoint/6,system_tracepoint/6,erlang_trace/3,on_load/0]).

-export([add_handler/3,delete_handler/2]).

-on_load(on_load/0).

-define(nif_stub,nif_stub_error(?LINE)).
-define(SERVER, lttng_trace_service).

-spec add_handler(atom(),binary(),[{mfa(),term()}]) -> ok.

add_handler(App, Cmd, [_|_] = Patterns) when is_atom(App), is_binary(Cmd) ->
    gen_server:call(?SERVER, {add_handler, App, Cmd, Patterns}, infinity).

-spec delete_handler(atom(),binary()) -> ok.

delete_handler(App, Cmd) when is_atom(App), is_binary(Cmd) ->
    gen_server:call(?SERVER, {delete_handler, App, Cmd}, infinity).


%% JUL interface according to JNI
%%
%% msg               Raw message provided by the JUL API.
%% logger_name       Logger name that trigger this event.
%% log_level         Log level of the event from JUL.
%% class_name        Name of the class that (allegedly) issued the logging request.
%% method_name       Name of the method that (allegedly) issued the logging request.
%% millis            Event time in milliseconds since 1970.
%% thread_id         Identifier for the thread where the message originated.

%% For Erlang/OTP we might want the following instead.
%%
%% msg               Raw message provided by the JUL API.
%% logger_name       Logger name that trigger this event.
%% log_level         Log level of the event from JUL.
%% module            Entrypoint
%% function          Entrypoint
%% arity             Entrypoint
%% millis            Event time in milliseconds since 1970.
%% process_id        Identifier for the thread where the message originated.

-spec user_tracepoint(atom(),atom(),atom(),integer(),integer(),term()) -> ok.

user_tracepoint(_Logger,_Module,_Function,_Millis,_LogLevel,_Msg) ->
    ?nif_stub.

-spec system_tracepoint(atom(),atom(),atom(),integer(),integer(),term()) -> ok.

system_tracepoint(_Logger,_Module,_Function,_Millis,_LogLevel,_Msg) ->
    ?nif_stub.

-spec erlang_trace(pid(),atom(),term()) -> ok.

erlang_trace(_Pid,_Type,_Msg) ->
    ?nif_stub.

%% aux

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

on_load() ->
    LibName = "lttng",
    PrivDir = code:priv_dir(lttng),
    Lib = filename:join([PrivDir, LibName]),
    erlang:load_nif(Lib,1).
