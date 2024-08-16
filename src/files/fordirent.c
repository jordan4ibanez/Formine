#include <dirent.h>
#include <stdio.h>

// You can thank:
// Jean-Bernard Jansen https://stackoverflow.com/a/4204758
void parse_directory_folders()
{
  DIR *d;
  struct dirent *dir;
  d = opendir(".");
  if (d)
  {
    while ((dir = readdir(d)) != NULL)
    {
      printf("%s\n", dir->d_name);
    }
    closedir(d);
  }
  return (0);
}