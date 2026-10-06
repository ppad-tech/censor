# TaskClock cannot be opened at the
# perf_event_paranoid level censor documents

Source: x86_64-linux runtime verification of
the channel-attribution counters added in
771a868 (censor 0.5.0), on an AMD EPYC 7713
KVM guest running NixOS 6.18.33 with
`kernel.perf_event_paranoid = 2`.

## Summary

`--meter task-clock` fails to start:

    task-clock meter unavailable: OpenFailed 13

errno 13 is `EACCES`. The counter is never
reached, so the task-clock rung of the
attribution ladder is unavailable on that
host. `ref-cycles` on the same host fails with
`OpenFailed 2` (`ENOENT`), which is the
documented microarchitecture-dependent
absence and not this issue; with both middle
rungs gone the ladder degrades to
instructions -> cycles -> wall.

## Diagnosis

`cbits/censor_perf.c` opens counter 4 with
`exclude_kernel = 0` and `exclude_hv = 0`,
deliberately, so that the reading includes
kernel time spent on the task's behalf --
which is what makes the task-clock /
ref-cycles gap isolate kernel and interrupt
time. But counting kernel time is privileged:
`perf_event_paranoid = 2` permits an
unprivileged process only user-space-only
counters, so the open is refused before the
event is ever consulted.

Confirmed directly rather than inferred. A
standalone `perf_event_open` on this host,
varying only the exclusion bits:

    task-clock  exclude_kernel=0 : -13
    task-clock  exclude_kernel=1 :   0
    ref-cycles  exclude_kernel=1 :  -2
    ref-cycles  exclude_kernel=0 : -13
    cpu-clock   exclude_kernel=1 :   0

So the software event exists and is openable;
only the kernel-time request is refused. Note
the paranoid check precedes the event-support
check -- `ref-cycles` reports `EACCES` rather
than its true `ENOENT` when asked for kernel
time.

TaskClock therefore needs
`perf_event_paranoid <= 1`, while every other
counter censor offers works at `2`. That is
the level the corpus's x86_64-linux sweeps run
at, and the level `Censor.Meter` documents as
sufficient.

## Resolution

Documentation, not behaviour. The `Counter`
and `withCounter` Haddocks and the README
ladder each stated or implied that TaskClock,
being a software counter, is available
wherever the hardware ones are not; they now
state the paranoid requirement and the errno
it fails with. `Censor.Runner.withMeterArg`
appends a hint naming `perf_event_paranoid` on
`EPERM`/`EACCES`, so the errno need not be
decoded by hand.

Retrying the open with `exclude_kernel = 1`
was considered and rejected. It would succeed
and return a user-only clock, which is not
what the rung measures: the task-clock /
ref-cycles gap would collapse to approximately
zero by construction and read as "no kernel
time" rather than "not measured". A rung that
is absent is recoverable; one that silently
lies is not.

Hosts that need the rung can lower the knob
(`sysctl kernel.perf_event_paranoid=1`), at
which point the counter opens normally.
