-module(lttng).

-export([tp_int/1,on_load/0]).

-on_load(on_load/0).
-define(LTTNG_NIF_VSN,001).

-define(nif_stub,nif_stub_error(?LINE)).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

on_load() ->
    LibName = "lttng",
    PrivDir = code:priv_dir(lttng),
    Lib = filename:join([PrivDir, LibName]),
    erlang:load_nif(Lib,1).

tp_int(_Int)-> ?nif_stub.
