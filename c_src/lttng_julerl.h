#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER lttng_julerl

#if !defined(LTTNG_JULERL_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define LTTNG_JULERL_H

#include <lttng/tracepoint.h>

/*
 * system events
 */
TRACEPOINT_EVENT(lttng_julerl, sys_event,
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
TRACEPOINT_EVENT(lttng_julerl, user_event,
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
TRACEPOINT_EVENT(lttng_julerl, erlang_trace,
    TP_ARGS(
	const char *, msg
    ),
    TP_FIELDS(
	ctf_string(msg, msg)
    )
)
#endif /* LTTNG_JULERL_H */

#undef TRACEPOINT_INCLUDE
#define TRACEPOINT_INCLUDE "./lttng_julerl.h"

/* This part must be outside protection */
#include <lttng/tracepoint-event.h>
