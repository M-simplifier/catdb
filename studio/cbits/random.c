#include <stddef.h>
#ifdef _WIN32
#include <windows.h>
#include <bcrypt.h>
int studio_random(void *buffer, int size) {
  return size > 0 && BCryptGenRandom(NULL, (PUCHAR)buffer, (ULONG)size,
    BCRYPT_USE_SYSTEM_PREFERRED_RNG) == 0;
}
#else
#include <stdio.h>
int studio_random(void *buffer, int size) {
  if (size <= 0) return 0;
  FILE *file = fopen("/dev/urandom", "rb");
  if (!file) return 0;
  size_t count = fread(buffer, 1, (size_t)size, file);
  fclose(file);
  return count == (size_t)size;
}
#endif
