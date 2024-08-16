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
  int file_count;
  int folder_count;
  bool *is_folder;
  int *string_lengths; // ARRAY_LENGTH
  char **strings;      // ARRAY_LENGTH
} for_dir;

//* This is POSIX only.
//* This enforces a path to end with "/".
char *check_ends_with_forward_slash(const char *path)
{
  int string_length;
  bool ends_with_slash;
  char *new_string;

  string_length = strlen(path);

  ends_with_slash = path[string_length - 1] == '/';

  if (ends_with_slash)
  {
    // We're doing this so we can free it regardless.
    new_string = malloc(sizeof(char[string_length]) + 1);
    strncpy(new_string, path, string_length);
    new_string[string_length] = '\0';
  }
  else
  {
    new_string = malloc(sizeof(char[string_length]) + 2);
    strncpy(new_string, path, string_length);
    new_string[string_length + 0] = '/';
    new_string[string_length + 1] = '\0';
  }
  return new_string;
}

//* Grab the files in a directory.
//* This is POSIX only.
//! todo: need a windows version.
for_dir *parse_directory_folders(const char *input_path)
{
  const char *path = check_ends_with_forward_slash(input_path);

  struct dirent *dir;
  for_dir *output = malloc(sizeof(for_dir));
  DIR *d = opendir(path);
  int count = 0;
  int string_length;

  output->is_folder = malloc(sizeof(bool[ARRAY_LENGTH]));
  output->string_lengths = malloc(sizeof(int[ARRAY_LENGTH]));
  output->strings = malloc(sizeof(char *[ARRAY_LENGTH]));

  output->array_length = count;

  if (d)
  {
    output->open_success = true;
    while ((dir = readdir(d)) != NULL)
    {
      //* Add +1 for null terminator.
      string_length = strlen(dir->d_name) + 1;

      // Allocate the string.
      char *allocated_string = malloc(sizeof(char[string_length]));

      // Use the safe version of strcpy.
      strncpy(allocated_string, &dir->d_name, string_length);

      //* Manually put the null terminator at the end of the string.
      allocated_string[string_length - 1] = '\0';

      // Now we assign.
      output->string_lengths[count] = string_length;
      output->strings[count] = allocated_string;

      // And make sure this thing doesn't blow up as we increment.
      count = count + 1;

      if (count >= ARRAY_LENGTH)
      {
        printf("[FORDIRENT C] SEVERE ERROR: More than [%i] files in path [%s]! BAILING.", ARRAY_LENGTH, path);
        break;
      }
    }

    //! Check if this works on windows cygwin.
    free(dir);

    closedir(d);
  }
  else
  {
    printf("[FORDIRENT C] SEVERE ERROR: Path [%s] failed to open.", path);
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

  // We must walk the string array like a caveman to free the heap pointers.
  for (int i = 0; i < ARRAY_LENGTH; i++)
  {
    // Don't allow this thing to walk into undefined behavior.
    if (output->strings[i] == NULL)
    {
      break;
    }
    free(output->strings[i]);
  }

  // Now free the string array.
  free(output->strings);

  // Next the string lengths array.
  free(output->string_lengths);

  // Also the is folder tracker.
  free(output->is_folder);

  // Then we can finally blow this thing up.
  free(output);

  output = NULL;

  return true;
}