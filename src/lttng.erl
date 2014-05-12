-module(lttng).

-export([user_tracepoint/6,system_tracepoint/6,erlang_trace/1,on_load/0]).

-on_load(on_load/0).

-define(nif_stub,nif_stub_error(?LINE)).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

on_load() ->
    LibName = "lttng",
    PrivDir = code:priv_dir(lttng),
    Lib = filename:join([PrivDir, LibName]),
    erlang:load_nif(Lib,1).

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


user_tracepoint(_Logger,_Module,_Function,_Millis,_LogLevel,_Msg) ->
    ?nif_stub.

system_tracepoint(_Logger,_Module,_Function,_Millis,_LogLevel,_Msg) ->
    ?nif_stub.

erlang_trace(_Msg) ->
    ?nif_stub.
