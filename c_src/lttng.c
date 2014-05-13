/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2013. All Rights Reserved.
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
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "erl_nif.h"

#define TRACEPOINT_DEFINE
#define TRACEPOINT_CREATE_PROBES
#include "lttng_julerl.h"


#define NAMELEN (256)
#define BUFLEN  (1024)

/* use domain lttng_jul to conform with Java */

static ERL_NIF_TERM am_ok;

static void init(ErlNifEnv *env) {
    /* useful atoms */
    am_ok         = enif_make_atom(env, "ok");
}

static ERL_NIF_TERM user_tracepoint(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char logger_name[NAMELEN];
    char module_name[NAMELEN];
    char function_name[NAMELEN];
    char msg[BUFLEN];
    long millis;
    int log_level;
    int thread_id = 1;
    ErlNifBinary ibin;

    if (argc == 6 &&
	    enif_get_atom(env,argv[0],logger_name,NAMELEN,ERL_NIF_LATIN1) &&
	    enif_get_atom(env,argv[1],module_name,NAMELEN,ERL_NIF_LATIN1) &&
	    enif_get_atom(env,argv[2],function_name,NAMELEN,ERL_NIF_LATIN1) &&
	    enif_get_long(env,argv[3],&millis) &&
	    enif_get_int(env,argv[4],&log_level) &&
	    enif_inspect_iolist_as_binary(env,argv[5],&ibin)) {

	size_t sz = ibin.size > BUFLEN - 1 ? BUFLEN - 1 : ibin.size;

	memcpy(msg,ibin.data,sz);
	msg[sz] = '\0';

	tracepoint(lttng_jul, user_event,
		msg, logger_name, module_name, function_name,
		millis, log_level, thread_id);

	return am_ok;
    }

    return enif_make_badarg(env);
}

static ERL_NIF_TERM system_tracepoint(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char logger_name[NAMELEN];
    char module_name[NAMELEN];
    char function_name[NAMELEN];
    char msg[BUFLEN];
    long millis;
    int log_level;
    int thread_id = 1;
    ErlNifBinary ibin;

    if (argc == 6 &&
	    enif_get_atom(env,argv[0],logger_name,NAMELEN,ERL_NIF_LATIN1) &&
	    enif_get_atom(env,argv[1],module_name,NAMELEN,ERL_NIF_LATIN1) &&
	    enif_get_atom(env,argv[2],function_name,NAMELEN,ERL_NIF_LATIN1) &&
	    enif_get_long(env,argv[3],&millis) &&
	    enif_get_int(env,argv[4],&log_level) &&
	    enif_inspect_iolist_as_binary(env,argv[5],&ibin)) {

	size_t sz = ibin.size > BUFLEN - 1 ? BUFLEN - 1 : ibin.size;

	memcpy(msg,ibin.data,sz);
	msg[sz] = '\0';

	tracepoint(lttng_jul, sys_event,
		msg, logger_name, module_name, function_name,
		millis, log_level, thread_id);

	return am_ok;
    }

    return enif_make_badarg(env);
}

static ERL_NIF_TERM erlang_trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char pid[NAMELEN];
    char type[NAMELEN];
    char msg[BUFLEN];

    erts_snprintf(pid,  NAMELEN, "%T", argv[0]);
    erts_snprintf(type, NAMELEN, "%T", argv[1]);
    erts_snprintf(msg,  BUFLEN,  "%T", argv[2]);
    tracepoint(lttng_jul, user_erlang_trace, pid, type, msg);
    return am_ok;
}

static ErlNifFunc nif_functions[] =  {
    { "erlang_trace", 3, erlang_trace },
    { "user_tracepoint", 6, user_tracepoint },
    { "system_tracepoint", 6, system_tracepoint }
};

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    init(env);
    *priv_data = NULL;
    return 0;
}

ERL_NIF_INIT(lttng, nif_functions, load, NULL, NULL, NULL)
