#include <dirent.h>
#include <stdio.h>
#include <stdbool.h>

/*
You can thank:
Jean-Bernard Jansen https://stackoverflow.com/a/4204758

This is built upon this, completely converted into fortran operable
implementation.
*/

typedef struct
{
  int array_length;
  char *strings[256];
} fort_dir;

fort_dir *parse_directory_folders(DIR *d)
{
  struct dirent *dir;
  fort_dir output;
  int count = 0;

  if (d)
  {
    while ((dir = readdir(d)) != NULL)
    {
      output.strings[count] = dir->d_name;
      count = count + 1;
    }
    closedir(d);
  }
  else
  {
    printf("uh oh\n");
  }

  output.array_length = count;

  //! Check if this works on windows.
  free(dir);

  return &output;
}