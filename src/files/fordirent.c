#include <stdlib.h>
#include <dirent.h>
#include <stdio.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

#ifdef __APPLE__
#include <string.h>
#endif

/*
You can thank:
Jean-Bernard Jansen https://stackoverflow.com/a/4204758
Frédéric Hamidi: https://stackoverflow.com/a/4553076
https://www.gnu.org/software/libc/manual/html_node/Testing-File-Type.html


This is built upon this, completely converted into fortran operable
implementation.

What this does is, basically creates a synchronized database of files and folders.
So if index 0 is a file named [test], it'll be set up like this in this micro database:
for_dir[0]->string = "test\0";
for_dir[0]->string_length = 4;
for_dir[0]->is_folder = false;

The other fields are mainly for it to be super easy to parse it in Fortran.
*/

//! This may not be enough, and it might blow up. :D
const int ARRAY_LENGTH = 1024;

//* This directly reflects the type in: [directory.f90]
typedef struct
{
  bool open_success;
  int array_length;
  int file_count;
  int folder_count;
  bool *is_folder;     // ARRAY_LENGTH
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
  int file_count = 0;
  int folder_count = 0;

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

//* This is POSIX only.
//* Grab the files and folders in a directory.
//! todo: need a windows version.
for_dir *parse_directory_folders(const char *input_path)
{
  const char *path = check_ends_with_forward_slash(input_path);
  // We want that null terminator.
  const int path_string_length = strlen(path) + 1;
  struct dirent *dir;
  for_dir *output = malloc(sizeof(for_dir));
  DIR *d = opendir(path);
  int total_entry_count = 0;
  int file_count = 0;
  int folder_count = 0;
  int string_length;

  output->is_folder = malloc(sizeof(bool[ARRAY_LENGTH]));
  output->string_lengths = malloc(sizeof(int[ARRAY_LENGTH]));
  output->strings = malloc(sizeof(char *[ARRAY_LENGTH]));

  output->array_length = total_entry_count;

  if (d)
  {
    output->open_success = true;
    while ((dir = readdir(d)) != NULL)
    {
      // We do not want these "folder names".
      // strcmp will return 0 for true.
      if (strcmp(dir->d_name, "..") == 0 || strcmp(dir->d_name, ".") == 0)
      {
        continue;
      }

      //* Add +1 for null terminator.
      string_length = strlen(dir->d_name) + 1;

      // Allocate the string.
      char *allocated_string = malloc(sizeof(char[string_length]));

      // Use the safe version of strcpy.
      strncpy(allocated_string, dir->d_name, string_length);

      //* Manually put the null terminator at the end of the string.
      allocated_string[string_length - 1] = '\0';

      // See if this is a file or a folder.
      // There are many things this can be, but we only care about the boolean.
      // Also this chunk may look scary, but it's mainly string manipulation.
      const char *temp_file_directory = malloc(sizeof(char[path_string_length + string_length]));
      strncpy(temp_file_directory, path, path_string_length);
      strcat(temp_file_directory, allocated_string);
      struct stat path_status;
      stat(temp_file_directory, &path_status);
      // S_ISDIR returns a non-zero if it's a folder.
      bool is_folder = S_ISDIR(path_status.st_mode) != 0;
      // Free this string.
      free(temp_file_directory);

      // Now we assign.
      output->is_folder[total_entry_count] = is_folder;
      output->string_lengths[total_entry_count] = string_length;
      output->strings[total_entry_count] = allocated_string;

      // Tick up the folder or file count.
      if (is_folder)
      {
        folder_count = folder_count + 1;
      }
      else
      {
        file_count = file_count + 1;
      }

      // And make sure this thing doesn't blow up as we increment.
      total_entry_count = total_entry_count + 1;

      if (total_entry_count >= ARRAY_LENGTH)
      {
        perror("opendir");
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

  output->array_length = total_entry_count;
  output->file_count = file_count;
  output->folder_count = folder_count;

  // The path has been customized with a malloc, free it.
  free(path);

  // Keep in mind, this is a malloc pointer, needs to be freed.
  return output;
}

//* We need this because we first must free the interior before the exterior
//* of the struct.
bool close_directory_folder_parse(for_dir *output)
{
  if (output == NULL)
  {
    printf("[FORDIRENT C] SEVERE ERROR: That's a null pointer!\n");
    return false;
  }

  // Free the is_folder tracker array.
  free(output->is_folder);

  // Next the string lengths array.
  free(output->string_lengths);

  // We must walk the string array like a caveman to free the heap pointers.
  for (int i = 0; i < output->array_length; i++)
  {
    // Don't allow this thing to walk into undefined behavior.
    if (output->strings[i] == NULL)
    {
      break;
    }
    // printf("%s\n", output->strings[i]);
    free(output->strings[i]);
  }

  // Now free the string array.
  free(output->strings);

  // Then we can finally blow this thing up.
  free(output);

  output = NULL;

  return true;
}