/* Linux perf_event_open shim for Censor.Meter's hardware-counter
 * meters. We count a single hardware event for the calling thread in
 * user space only (exclude_kernel), which is permitted to unprivileged
 * processes at kernel.perf_event_paranoid <= 2.
 *
 * The counter is thread-affine (pid == 0 binds it to the opening
 * thread), so the Haskell side opens it and runs every measurement on
 * the same bound OS thread.
 */

#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <linux/perf_event.h>
#include <asm/unistd.h>

static long censor_perf_event_open(struct perf_event_attr *attr, pid_t pid,
                                   int cpu, int group, unsigned long flags) {
  return syscall(__NR_perf_event_open, attr, pid, cpu, group, flags);
}

/* counter: 0 = instructions retired, 1 = branch instructions retired.
 * Returns a file descriptor (>= 0) on success, or -errno on failure. */
int censor_perf_open(int counter) {
  struct perf_event_attr attr;
  memset(&attr, 0, sizeof(attr));
  attr.type           = PERF_TYPE_HARDWARE;
  attr.size           = sizeof(attr);
  attr.config         = (counter == 1) ? PERF_COUNT_HW_BRANCH_INSTRUCTIONS
                                        : PERF_COUNT_HW_INSTRUCTIONS;
  attr.disabled       = 1;
  attr.exclude_kernel = 1;
  attr.exclude_hv     = 1;

  long fd = censor_perf_event_open(&attr, 0 /* this thread */,
                                   -1 /* any cpu */, -1, 0);
  if (fd < 0) return -errno;
  return (int) fd;
}

/* Reset and arm the counter immediately before a measured region. */
void censor_perf_begin(int fd) {
  ioctl(fd, PERF_EVENT_IOC_RESET, 0);
  ioctl(fd, PERF_EVENT_IOC_ENABLE, 0);
}

/* Disarm and read the accumulated count after a measured region. */
uint64_t censor_perf_end(int fd) {
  ioctl(fd, PERF_EVENT_IOC_DISABLE, 0);
  uint64_t value = 0;
  ssize_t n = read(fd, &value, sizeof(value));
  (void) n;
  return value;
}

void censor_perf_close(int fd) {
  close(fd);
}
