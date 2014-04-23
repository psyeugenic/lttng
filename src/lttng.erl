-module(lttng).

-export([user_tracepoint/6,system_tracepoint/6,on_load/0]).
-export([go/0]).

-on_load(on_load/0).

-define(nif_stub,nif_stub_error(?LINE)).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

on_load() ->
    LibName = "lttng",
    PrivDir = code:priv_dir(lttng),
    Lib = filename:join([PrivDir, LibName]),
    erlang:load_nif(Lib,1).

user_tracepoint(_Logger,_Module,_Function,_Millis,_LogLevel,_Msg) ->
    ?nif_stub.

system_tracepoint(_Logger,_Module,_Function,_Millis,_LogLevel,_Msg) ->
    ?nif_stub.

go() ->
    lttng_service:start_link().
