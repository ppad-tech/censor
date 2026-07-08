/* Portable dlopen/dlsym/dlclose/dlerror shim for Censor.Runner.DL.
 * Baked flags: RTLD_NOW so unresolved symbols surface at open time;
 * RTLD_LOCAL so the library's symbols do not leak into the global
 * scope. Both constants differ in value between glibc and Darwin, so
 * the flag word is set here rather than on the Haskell side. */

#include <dlfcn.h>

void *censor_dlopen(const char *path) {
  return dlopen(path, RTLD_NOW | RTLD_LOCAL);
}

void *censor_dlsym(void *handle, const char *name) {
  return dlsym(handle, name);
}

int censor_dlclose(void *handle) {
  return dlclose(handle);
}

const char *censor_dlerror(void) {
  return dlerror();
}
