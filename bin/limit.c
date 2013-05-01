#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>

/*********************************************************************
 * Adjust the following for programs which require higher limits.
 *
 * Generally the MAX_*_SEC and MAX_SIZES variables will be the ones
 * which need to be increased for long running programs and programs
 * which deal with large amounts of data respectively.
 ********************************************************************/
#define MAX_CPU_SEC        1
#define MAX_WALL_SEC       4
#define MAX_NUM_PROCS      512
#define MAX_NUM_FILE_HNDLS 512
#define MAX_SIZES          65535

int main(int argc, char *argv[]) {
  struct rlimit limit;
  limit.rlim_cur = MAX_CPU_SEC; limit.rlim_max = MAX_CPU_SEC;
  setrlimit(RLIMIT_CPU, &limit);     /* cpu seconds */
  limit.rlim_cur = MAX_NUM_PROCS; limit.rlim_max = MAX_NUM_PROCS;
  setrlimit(RLIMIT_NPROC, &limit);   /* number of spawned processes */
  limit.rlim_cur = MAX_NUM_FILE_HNDLS; limit.rlim_max = MAX_NUM_FILE_HNDLS;
  setrlimit(RLIMIT_NOFILE, &limit);  /* number of open files */
  limit.rlim_cur = MAX_SIZES; limit.rlim_max = MAX_SIZES;
  setrlimit(RLIMIT_FSIZE, &limit);   /* max file size (bytes) */
  setrlimit(RLIMIT_MEMLOCK, &limit); /* max memory locked into RAM (bytes) */
  setrlimit(RLIMIT_STACK, &limit);   /* max stack size (bytes) */
  alarm(MAX_WALL_SEC);               /* wall clock seconds */

  if(argc>1)
    { return execvp(argv[1], &argv[1]); }
  else
    { printf("No arguments supplied to `%s'.\n", argv[0]); return 1; }
}
