LTTng: Linux Trace Toolkit - next generation
--------------------------------------------

This Erlang application, `lttng`, leverages the LTTng-UST JUL (Java Utility Logging)
interface to enable and disable Erlang call tracing.
It pushes trace-data to LTTng's ConsumerD via tracepoints implemented in NIFs.

*Note:* This application is a prototype and should be considered as such.

Erlang has its own logging framework, the `error_logger`, which would probably
be a better fit for the JUL interface but in this prototype we use it to define
UST tracepoints instead.

#### Requirements ####

LTTng needs developer libs. You should compile and install:

* first liburcu, `git clone git://git.lttng.org/userspace-rcu.git`
* then LTTng-UST, `git clone git://git.lttng.org/lttng-ust.git`
* then LTTng-tools. `git clone git://git.lttng.org/lttng-tools.git`

For details, see http://lttng.org/download

#### Build ####

To build, simply:

    $ git clone git@github.com:psyeugenic/lttng.git
    $ cd lttng
    $ make

Make will fetch `rebar` via wget and then compile the source files.

#### Start ####

Start Erlang with the lttng application in the library path and add a handler.

In Erlang:

    > application:start(lttng).
    ok
    > lttng:add_handler(myapp, <<"user1">>, [{{erlang,display,1},true}]).
    ok


The handler will specify the application and a user-defined string that is
associated with a set of match specification for call tracing.


#### Use lttng ####

In bash shell:

    $ lttng create erlang
    Session erlang created.
    Traces will be written in /home/egil/lttng-traces/erlang-20140513-180105

    $ lttng list -j
    JUL events (Logger name):
    -------------------------

    PID: 14424 - Name: /opt/installs/lib/erlang/erts-6.0/bin/beam.smp
          - myapp:user1 [disabled]

    $ lttng enable-event -j myapp:user1
    JUL event myapp:user1 created in channel channel0

    $ lttng start
    Tracing started for session erlang

In Erlang shell:

    > erlang:display(wat).
    wat
    true

In bash shell:
    
    $ lttng stop
    Waiting for data availability
    Tracing stopped for session erlang

    $ lttng view
    Trace directory: /home/egil/lttng-traces/erlang-20140513-180105

    [18:02:14.810789887] (+?.?????????) palantir lttng_jul:user_erlang_trace: { cpu_id = 3 }, { pid = "<0.33.0>", type = "call", msg = "{erlang,display,[wat]}" }
