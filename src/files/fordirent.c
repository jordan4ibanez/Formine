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
  bool open_success;
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
    output.open_success = true;
    while ((dir = readdir(d)) != NULL)
    {
      output.strings[count] = dir->d_name;
      count = count + 1;
    }
    closedir(d);
  }
  else
  {
    output.open_success = false;
  }

  output.array_length = count;

  //! Check if this works on windows.
  free(dir);

  // Keep in mind, this is a pointer, to the stack.
  return &output;
}