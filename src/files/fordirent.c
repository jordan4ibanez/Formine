#include <dirent.h>
#include <stdio.h>
#include <stdbool.h>

/*
You can thank:
Jean-Bernard Jansen https://stackoverflow.com/a/4204758

This is built upon this, completely converted into fortran operable
implementation.
*/

const int ARRAY_LENGTH = 1024;

//* This directly reflects the type in: [directory.f90]
typedef struct
{
  bool open_success;
  int array_length;
  char **strings; // ARRAY_LENGTH
} for_dir;

//* Grab the files in a directory.
for_dir *parse_directory_folders(const char *path)
{
  struct dirent *dir;
  for_dir *output = malloc(sizeof(for_dir));
  output->strings = malloc(sizeof(char *[ARRAY_LENGTH]));
  DIR *d = opendir(path);

  int count = 0;
  output->array_length = count;

  if (d)
  {
    output->open_success = true;
    while ((dir = readdir(d)) != NULL)
    {
      output->strings[count] = dir->d_name;
      // printf("%s\n", output->strings[count]);
      count = count + 1;
      if (count >= ARRAY_LENGTH)
      {
        printf("[FORDIRENT C] SEVERE ERROR: More than [%i] files in path [%s]! BAILING.", ARRAY_LENGTH, path);
      }
    }

    //! Check if this works on windows cygwin.
    free(dir);

    closedir(d);
  }
  else
  {
    printf("OH NO\n");
    output->open_success = false;
  }

  output->array_length = count;

  // Keep in mind, this is a malloc pointer, needs to be freed.
  return output;
}

// We need this because we first must free the interior before the exterior
// of the struct.
bool close_directory_folder_parse(for_dir *output)
{
  if (output == NULL)
  {
    printf("[FORDIRENT C] SEVERE ERROR: That's a null pointer!\n");
    return false;
  }

  free(output->strings);
  free(output);

  output = NULL;

  return true;
}