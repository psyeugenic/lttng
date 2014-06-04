/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER lttng_jul

#if !defined(LTTNG_JULERL_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define LTTNG_JULERL_H

#include <lttng/tracepoint.h>

/*
 * system events
 * use domain lttng_jul to conform with Java
 */
TRACEPOINT_EVENT(lttng_jul, sys_event,
    TP_ARGS(
	const char *, msg,
	const char *, logger_name,
	const char *, module_name,
	const char *, function_name,
	long, millis,
	int, log_level,
	int, thread_id),
    TP_FIELDS(
	ctf_string(msg, msg)
	ctf_string(logger_name, logger_name)
	ctf_string(module_name, module_name)
	ctf_string(function_name, function_name)
	ctf_integer(long, long_millis, millis)
	ctf_integer(int, int_loglevel, log_level)
	ctf_integer(int, int_threadid, thread_id)
    )
)

/*
 * user events
 */
TRACEPOINT_EVENT(lttng_jul, user_event,
    TP_ARGS(
	const char *, msg,
	const char *, logger_name,
	const char *, module_name,
	const char *, function_name,
	long, millis,
	int, log_level,
	int, thread_id),
    TP_FIELDS(
	ctf_string(msg, msg)
	ctf_string(logger_name, logger_name)
	ctf_string(module_name, module_name)
	ctf_string(function_name, function_name)
	ctf_integer(long, long_millis, millis)
	ctf_integer(int, int_loglevel, log_level)
	ctf_integer(int, int_threadid, thread_id)
    )
)

/*
 * erlang trace
 */
TRACEPOINT_EVENT(lttng_jul, user_erlang_trace,
    TP_ARGS(
	const char *, pid,
	const char *, type,
	const char *, msg
    ),
    TP_FIELDS(
	ctf_string(pid, pid)
	ctf_string(type, type)
	ctf_string(msg, msg)
    )
)
#endif /* LTTNG_JULERL_H */

#undef TRACEPOINT_INCLUDE
#define TRACEPOINT_INCLUDE "./lttng_julerl.h"

/* This part must be outside protection */
#include <lttng/tracepoint-event.h>
