/* Host-environment shims for Censor.Runner.Env: load average, kernel
 * release, a UTC timestamp, and (on Darwin) the CPU model string.
 *
 * Everything is best-effort: each call returns 0 (or, for loadavg,
 * the sample count) on success and -1 when the facility is absent on
 * the host, and the Haskell side maps failure to Nothing. On Windows
 * every call fails; the fields simply stay unpopulated there.
 */

#include <stddef.h>

#if !defined(_WIN32)

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <sys/utsname.h>

#if defined(__APPLE__)
#include <sys/sysctl.h>
#endif

int censor_env_loadavg(double *out) {
  return getloadavg(out, 3);
}

/* via sysconf, not GHC's getNumProcessors: the latter reports 1 on
 * the non-threaded RTS. */
int censor_env_cores(void) {
  long n = sysconf(_SC_NPROCESSORS_ONLN);
  if (n < 1) return -1;
  return (int) n;
}

int censor_env_kernel(char *buf, size_t n) {
  struct utsname u;
  if (uname(&u) != 0) return -1;
  if ((size_t) snprintf(buf, n, "%s", u.release) >= n) return -1;
  return 0;
}

int censor_env_time(char *buf, size_t n) {
  time_t t = time(NULL);
  struct tm tm;
  if (gmtime_r(&t, &tm) == NULL) return -1;
  if (strftime(buf, n, "%Y-%m-%dT%H:%M:%SZ", &tm) == 0) return -1;
  return 0;
}

int censor_env_cpu(char *buf, size_t n) {
#if defined(__APPLE__)
  size_t len = n;
  if (sysctlbyname("machdep.cpu.brand_string", buf, &len, NULL, 0) != 0)
    return -1;
  return 0;
#else
  /* on Linux the Haskell side reads /proc/cpuinfo instead. */
  (void) buf; (void) n;
  return -1;
#endif
}

#else

int censor_env_loadavg(double *out)          { (void) out; return -1; }
int censor_env_cores(void)                   { return -1; }
int censor_env_kernel(char *buf, size_t n)   { (void) buf; (void) n; return -1; }
int censor_env_time(char *buf, size_t n)     { (void) buf; (void) n; return -1; }
int censor_env_cpu(char *buf, size_t n)      { (void) buf; (void) n; return -1; }

#endif
