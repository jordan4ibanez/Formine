#ifdef __MINGW32__
#define STB_IMAGE_IMPLEMENTATION
#include "/msys64/ucrt64/include/stb/stb_image.h"
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "/msys64/ucrt64/include/stb/stb_image_write.h"
#endif