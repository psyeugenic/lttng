#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_erlang_trace

#undef TRACEPOINT_INCLUDE
#define TRACEPOINT_INCLUDE "com_ericsson_erlang_trace.h"

#if !defined(COM_ERICSSON_ERLANG_TRACE_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define COM_ERICSSON_ERLANG_TRACE_H

#include <lttng/tracepoint.h>

TRACEPOINT_EVENT(com_ericsson_erlang_trace,tp_str,
                 TP_ARGS(char*, str
                         ),
                 TP_FIELDS(
		     ctf_string(trace_term, str)
                           )
                 )

TRACEPOINT_LOGLEVEL(com_ericsson_erlang_trace,tp_str,TRACE_WARNING)

#endif /* COM_ERICSSON_ERLANG_TRACE_H */

/* This part must be outside protection */
#include <lttng/tracepoint-event.h>

