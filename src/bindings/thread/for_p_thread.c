#include <pthread.h>
// This one is for looking up error IDs.
// #include <errno.h>
// Go through to <errno-base.h>

// You can ctrl - click this to get there.
// int test = ENOENT;

const static int pthread_attr_t_size = sizeof(pthread_attr_t);

//* Make this portable.
int for_p_thread_get_pthread_attr_t_width()
{
  return pthread_attr_t_size;
}