/* Test shim for the `censor` executable.
 *
 * Build (macOS):
 *   cc -O2 -shared -fPIC -o shim.dylib shim.c
 *
 * Build (Linux):
 *   cc -O2 -shared -fPIC -o shim.so shim.c
 *
 * Two targets, both taking a 64-byte workspace laid out as
 *   [a (32B) | b (32B)]
 * and returning nothing (matching censor's `Ptr Word8 -> IO ()` ABI).
 *
 *   ct_countdown      constant-iteration hot loop; timing does not
 *                     depend on the workspace bytes. Used as a
 *                     positive control (should PASS).
 *
 *   leaky_countdown   loop iteration count is a linear function of
 *                     workspace byte 0. Under `--secret 0:32`, class
 *                     A pins byte 0 to a fixed value (so a fixed
 *                     iteration count) while class B draws byte 0
 *                     uniformly per pair (so a variable iteration
 *                     count with mean 127.5). Used as a negative
 *                     control (should REJECT).
 *
 * The `volatile` sink prevents the compiler from folding the loop
 * body to a constant.
 */

#include <stdint.h>

void ct_countdown(uint8_t *ws) {
  volatile uint64_t sink = 0;
  uint64_t n = 25500;         /* 255 * 100, matches leaky's max */
  (void)ws;
  while (n--) sink += n;
}

void leaky_countdown(uint8_t *ws) {
  volatile uint64_t sink = 0;
  uint64_t n = (uint64_t)ws[0] * 100;
  while (n--) sink += n;
}
