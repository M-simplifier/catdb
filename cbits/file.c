/* The only platform boundary: exclusive ownership, exact IO and durable flush.
   All schema, query, transaction and log semantics live in Haskell. */
#include <stdint.h>
#include <stddef.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#ifdef _WIN32
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#include <share.h>
#include <sys/stat.h>
#include <stdlib.h>
#else
#include <unistd.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/stat.h>
#endif

int catdb_open(const char *path, int create, int *err) {
  int fd;
#ifdef _WIN32
  int n = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1, NULL, 0);
  if (!n) { *err = EINVAL; return -1; }
  wchar_t *wide = (wchar_t *)malloc((size_t)n * sizeof(wchar_t));
  if (!wide) { *err = ENOMEM; return -1; }
  MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1, wide, n);
  errno_t e = _wsopen_s(&fd, wide, _O_RDWR | _O_BINARY | _O_NOINHERIT |
                       (create ? _O_CREAT | _O_EXCL : 0), _SH_DENYNO, _S_IREAD | _S_IWRITE);
  free(wide);
  if (e) { *err = (int)e; return -1; }
  OVERLAPPED ov = {0};
  if (!LockFileEx((HANDLE)_get_osfhandle(fd), LOCKFILE_EXCLUSIVE_LOCK | LOCKFILE_FAIL_IMMEDIATELY,
                 0, 1, 0, &ov)) {
    DWORD code = GetLastError(); _close(fd);
    *err = code == ERROR_LOCK_VIOLATION ? -1 : (int)code;
    return -1;
  }
#else
  fd = open(path, O_RDWR | O_CLOEXEC | (create ? O_CREAT | O_EXCL : 0), 0600);
  if (fd < 0) { *err = errno; return -1; }
  if (flock(fd, LOCK_EX | LOCK_NB) < 0) {
    int code = errno; close(fd);
    *err = (code == EWOULDBLOCK || code == EAGAIN) ? -1 : code; return -1;
  }
#endif
  *err = 0; return fd;
}

int catdb_close(int fd) {
#ifdef _WIN32
  return _close(fd) == 0 ? 0 : errno;
#else
  return close(fd) == 0 ? 0 : errno;
#endif
}

int catdb_seek(int fd, int64_t offset) {
#ifdef _WIN32
  return _lseeki64(fd, offset, SEEK_SET) < 0 ? errno : 0;
#else
  return lseek(fd, (off_t)offset, SEEK_SET) < 0 ? errno : 0;
#endif
}

int catdb_size(int fd, int64_t *size) {
#ifdef _WIN32
  struct _stat64 st;
  if (_fstat64(fd, &st)) return errno;
#else
  struct stat st;
  if (fstat(fd, &st)) return errno;
#endif
  *size = (int64_t)st.st_size; return 0;
}

int catdb_read(int fd, void *buffer, size_t length) {
  unsigned char *p = (unsigned char *)buffer;
  while (length) {
    unsigned int chunk = (unsigned int)(length > INT_MAX ? INT_MAX : length);
#ifdef _WIN32
    int n = _read(fd, p, chunk);
#else
    ssize_t n = read(fd, p, chunk);
#endif
    if (n < 0) { if (errno == EINTR) continue; return errno; }
    if (n == 0) return EIO;
    p += n; length -= (size_t)n;
  }
  return 0;
}

int catdb_write(int fd, const void *buffer, size_t length) {
  const unsigned char *p = (const unsigned char *)buffer;
  while (length) {
    unsigned int chunk = (unsigned int)(length > INT_MAX ? INT_MAX : length);
#ifdef _WIN32
    int n = _write(fd, p, chunk);
#else
    ssize_t n = write(fd, p, chunk);
#endif
    if (n < 0) { if (errno == EINTR) continue; return errno; }
    if (n == 0) return EIO;
    p += n; length -= (size_t)n;
  }
  return 0;
}

int catdb_sync(int fd) {
#ifdef _WIN32
  return _commit(fd) == 0 ? 0 : errno;
#else
  return fsync(fd) == 0 ? 0 : errno;
#endif
}

int catdb_truncate(int fd, int64_t size) {
#ifdef _WIN32
  return (int)_chsize_s(fd, size);
#else
  return ftruncate(fd, (off_t)size) == 0 ? 0 : errno;
#endif
}
