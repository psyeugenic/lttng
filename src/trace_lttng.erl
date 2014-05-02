-module(trace_lttng).

-export([start/0]).

start() ->
    Driver = "trace_lttng_drv",
    Dir1 = code:priv_dir(lttng),
    ok = erl_ddll:load_driver(Dir1, Driver),
    %%L = lists:flatten(io_lib:format("~s ~p ~p 2", [Driver, Portno, Qsiz])),
    open_port({spawn, Driver}, [eof]).
