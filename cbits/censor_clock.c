/* Tick-resolved monotonic clock for Darwin.
 *
 * GHC's getMonotonicTimeNSec dispatches to
 * clock_gettime(CLOCK_MONOTONIC), which macOS quantises to
 * microseconds. That collapses sub-microsecond structure and
 * concentrates wall readings onto 1000 ns atoms, which in turn
 * lands warmup-calibrated CDF cut points exactly on quantisation
 * boundaries (see issues/ISSUE2.md). CLOCK_UPTIME_RAW is backed
 * by the hardware counter (the mach_absolute_time time base) and
 * resolves at its tick rate -- ~42 ns on 24 MHz Apple Silicon
 * counters, finer where the counter runs faster.
 */
#if defined(__APPLE__)

#include <stdint.h>
#include <time.h>

uint64_t censor_monotonic_ns(void) {
  return clock_gettime_nsec_np(CLOCK_UPTIME_RAW);
}

#endif
