%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(trace_lttng).

-export([start/0]).

start() ->
    Driver = "trace_lttng_drv",
    Dir1 = code:priv_dir(lttng),
    ok = erl_ddll:load_driver(Dir1, Driver),
    %%L = lists:flatten(io_lib:format("~s ~p ~p 2", [Driver, Portno, Qsiz])),
    open_port({spawn, Driver}, [eof]).
