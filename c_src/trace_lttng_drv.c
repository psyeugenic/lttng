/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2011. All Rights Reserved.
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

/*
 * Purpose: Send trace messages to a file.
 */

#ifdef __WIN32__
#include <windows.h>
#endif
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __WIN32__
#  include <io.h>
#  define write _write
#  define close _close
#  define unlink _unlink
#else
#  include <unistd.h>
#endif
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>

#include <ei.h> /* erl_interface */

/*
 * Deduce MAXPATHLEN, which is the one to use in this file, 
 * from any available definition.
 */
#ifndef MAXPATHLEN
#  ifdef PATH_MAX /* Posix */
#    define MAXPATHLEN PATH_MAX
#  else
#    ifdef _POSIX_PATH_MAX /* Posix */
#      define MAXPATHLEN _POSIX_PATH_MAX
#    else
#      ifdef MAXPATH
#        define MAXPATHLEN MAXPATH
#      else
#        ifdef MAX_PATH
#          define MAXPATHLEN MAX_PATH
#        else
#          ifdef _MAX_PATH
#            define MAXPATHLEN _MAX_PATH
#         else
#            error Could not define MAXPATHLEN
#          endif
#        endif
#      endif
#    endif
#  endif
#endif



#ifdef DEBUG
#  ifndef __WIN32__
#    define ASSERT(X) do {if (!(X)) {erl_exit(1,"%s",#X);} } while(0)
#  else
#    include <assert.h>
#    define ASSERT(X) assert(X)
#  endif
#else
#  define ASSERT(X)
#endif



#include "erl_driver.h"


#define TRACEPOINT_DEFINE
#define TRACEPOINT_CREATE_PROBES
//#define TRACEPOINT_PROBE_DYNAMIC_LINKAGE
#include "com_ericsson_erlang_trace.h"


/*
** Protocol from driver:
** '\0' -> ok
** '\1' ++ String -> {error, Atom}
**
** Protocol when opening (arguments to start):
** ["w <WrapSize> <WrapCnt> <TailIndex> "] "n <Filename>"
** Where...
** <Filename>, a string ('\0' terminated):
**    The filename where the trace output is to be written.
** "w ...", if present orders a size limited wrapping log.
** <WrapSize>, an unsigned integer:
**    The size limit of each log file.
** <WrapCnt>, an unsigned integer:
**    The number of log files.
** <TailIndex>, an unsigned integer:
**    The (zero based) index of where to insert the filename
**    sequence count "a".."z","aa".."az","ba".."zz","aaa"...
**
** Port control messages handled:
** 'f' -> '\0' (ok) | '\1' ++ String (error) : Flush file.
**
** The package written to the file looks like this:
** +--+--------+-----------------------------------+
** |Op|Size NBO|Term in external format or empty   |
** +--+--------+-----------------------------------+
** Op, a char, for conformance with the IP driver:
**    0 = binary, 1 = drop
**    If Op is 1, then Size reflects the number of dropped messages. The 
**    op 1 is never used in this driver.
** Size, a 32 bit interger in network byte order:
**    Either the size of the binary term, or the number of packet's dropped.
** Term, an array of bytes:
**    An erlang term in the external format or simply empty if Op == 1, the
**    term is Size long.
*/ 


typedef struct trace_lttng_data {
    int what_ever;
} TraceLTTngData;

static TraceLTTngData the_data; 

/*
** Interface routines
*/
static ErlDrvData trace_lttng_start(ErlDrvPort port, char *buff);
static void trace_lttng_stop(ErlDrvData handle);
static void trace_lttng_output(ErlDrvData handle, char *buff,
			      ErlDrvSizeT bufflen);

#ifdef __WIN32__
static int win_open(char *path, int flags, int mask);
#  define open win_open
#else
ErlDrvEntry *driver_init(void);
#endif

/*
** The driver struct
*/
ErlDrvEntry trace_lttng_driver_entry = {
    NULL,		   /* F_PTR init, N/A */
    trace_lttng_start,      /* L_PTR start, called when port is opened */
    trace_lttng_stop,       /* F_PTR stop, called when port is closed */
    trace_lttng_output,     /* F_PTR output, called when erlang has sent */
    NULL,                  /* F_PTR ready_input, called when input descriptor 
			      ready */
    NULL,                  /* F_PTR ready_output, called when output 
			      descriptor ready */
    "trace_lttng_drv",      /* char *driver_name, the argument to open_port */
    NULL, /* trace_lttng_finish */
    NULL,                  /* void * that is not used (BC) */
    NULL, /* trace_lttng_control */
    NULL, /* trace_lttng_timeout */
    NULL,                  /* F_PTR outputv, reserved */
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL,
};

/*
** Driver initialization routine
*/
DRIVER_INIT(trace_lttng_drv)
{
    return &trace_lttng_driver_entry;
}

/*
** Driver interface routines
*/

/*
** Open a port
*/
static ErlDrvData trace_lttng_start(ErlDrvPort port, char *buff)
{


#ifdef HARDDEBUG
    fprintf(stderr,"trace_lttng_start (%s)\r\n", buff);
#endif
#if 0
    static const char name[] = "trace_lttng_drv";

    w = 0; /* Index of where sscanf gave up */
    size = 0; /* Warning elimination */
    cnt = 0;  /* -""- */
    time = 0;  /* -""- */
    tail = 0; /* -""- */
    n = sscanf(buff, "trace_lttng_drv %n w %u %u %u %u %n",
	       &w, &size, &cnt, &time, &tail, &w);

    if (w < sizeof(name) || (n != 0 && n != 4))
	return ERL_DRV_ERROR_BADARG;
#endif

    return (ErlDrvData) &the_data;
}


/*
** Close a port
*/
static void trace_lttng_stop(ErlDrvData handle)
{
}

/*
** Data sent from erlang to port.
*/
static void trace_lttng_output(ErlDrvData handle, char *buff,
			      ErlDrvSizeT bufflen)
{
    /*TraceLTTngData *data = (TraceLTTngData *) handle;*/
    int index = 0;
    char* output = malloc(BUFSIZ);

    ei_s_print_term(&output, buff+1, &index);

    tracepoint(com_ericsson_erlang_trace,tp_str,output);
}


