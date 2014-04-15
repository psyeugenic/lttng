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
#define TRACEPOINT_PROBE_DYNAMIC_LINKAGE
#include "com_ericsson_otp.h" // Must be defined after the two #define statements




static ERL_NIF_TERM tp_int(ErlNifEnv* env, int argc,
	const ERL_NIF_TERM argv[]) {
  long int myLong;
  if (argc == 1) {
    if (enif_get_long(env,argv[0],&myLong)) {
      tracepoint(com_ericsson_otp,tp_int,(int) myLong);
      return enif_make_long(env,myLong+1);
    }
    else {
      return enif_make_atom(env, "error");
    }}
  else {
    return enif_make_atom(env, "error");
  }
}

static ErlNifFunc nif_funcs[] =  {
    { "tp_int", 1, tp_int },
};

ERL_NIF_INIT(lttng, nif_funcs, NULL, NULL, NULL, NULL)
