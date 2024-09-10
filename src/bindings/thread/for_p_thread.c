#include <pthread.h>
#include <unistd.h>
// This one is for looking up error IDs.
// #include <errno.h>
// Go through to <errno-base.h>

// You can ctrl - click this to get there.
// int test = ENOENT;

const static int pthread_mutex_t_size = sizeof(pthread_mutex_t);

const static int pthread_create_detached_id = PTHREAD_CREATE_DETACHED;

//* Make this portable.

int for_p_thread_get_pthread_mutex_t_width()
{
  return pthread_mutex_t_size;
}

//* Getting the number of available threads.
//* You can thank tavianator: https://www.reddit.com/r/C_Programming/comments/6zxnr1/comment/dmzuwt6
int for_p_thread_get_cpu_threads()
{
  int thread_count = sysconf(_SC_NPROCESSORS_ONLN);
  thread_count = thread_count - 1;
  if (thread_count == 0)
  {
    thread_count = 1;
  }
  return thread_count;
}