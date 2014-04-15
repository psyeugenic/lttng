#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_otp

#undef TRACEPOINT_INCLUDE
/*#define TRACEPOINT_INCLUDE "/home/uabkeld/erl_lib/lib/lttng/c_src/com_ericsson_otp.h"
 */
#define TRACEPOINT_INCLUDE "./com_ericsson_otp.h"

#if !defined(COM_ERICSSON_OTP_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define COM_ERICSSON_OTP_H

#include <lttng/tracepoint.h>

TRACEPOINT_EVENT(com_ericsson_otp,tp_int,
		 TP_ARGS(int, MyInt
			 ),
		 TP_FIELDS(
			   ctf_integer(int,MyInt,MyInt)
			   )
		 )

TRACEPOINT_LOGLEVEL(com_ericsson_otp,tp_int,TRACE_WARNING)

#endif /* COM_ERICSSON_OTP_H */

/* This part must be outside protection */
#include <lttng/tracepoint-event.h>

