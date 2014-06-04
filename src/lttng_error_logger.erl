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

%%
%% File:    lttng_error_logger.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-04-24
%%

-module(lttng_error_logger).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([add_handler/0, add_handler/1]).

-record(state,{}).

init([])-> {ok, #state{}}.

add_handler() -> add_handler([]).
add_handler(Args) -> error_logger:add_report_handler(?MODULE, Args).

%handle_event({error, _Leader, {Pid, Msg, Data}}, S) ->
%    {ok, S};
%handle_event({info_msg, _Leader, {Pid, Msg, Data}}, S) ->
%    {ok, S};
%handle_event({warning_msg, _Leader, {Pid, Msg, Data}}, S) ->
%    {ok, S};

handle_event({Type, _Leader, {_Pid, Msg, _Data}}, S) ->
    {M,F,_} = get_mfa(),
    Millis = timestamp_to_millis(),
    lttng:user_tracepoint(Type,M,F,Millis,800,Msg),
    {ok, S}.

%handle_event({error_report, _Leader, _}, S) ->
%    {ok, S};
%handle_event({info_report, _Leader, _}, S) ->
%    {ok, S};
%handle_event({warning_report, _Leader, _}, S) ->
%    {ok, S}.

handle_call(_Request, S) ->
    Reply = ok,
    {ok, Reply, S}.

handle_info(_Info, S) ->
    {ok, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%% aux

timestamp_to_millis() ->
    timestamp_to_millis(os:timestamp()).
timestamp_to_millis({Ms,S,Us}) ->
    (1000000000 * Ms) + (1000 * S) + (Us div 1000).

get_mfa() -> {mod,func,0}.
