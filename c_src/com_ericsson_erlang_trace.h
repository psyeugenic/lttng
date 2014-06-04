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

