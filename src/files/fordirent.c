#include <dirent.h>
#include <stdio.h>

/*
You can thank:
Jean-Bernard Jansen https://stackoverflow.com/a/4204758

This is built upon this, completely converted into fortran operable
implementation.
*/

// DIR *open_directory(const char *path)
// {

//   printf("%s\n",path);

//   return NULL;
// }

void parse_directory_folders(DIR *d)
{

  struct dirent *dir;

  if (d)
  {
    while ((dir = readdir(d)) != NULL)
    {
      printf("%s\n", dir->d_name);
    }
    closedir(d);
  }
  else
  {
    printf("uh oh\n");
  }
  return (0);
}