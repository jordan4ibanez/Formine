### 1) Get the native gfortran compiler

(I use the equation.com one)
https://fortran-lang.org/learn/os_setup/install_gfortran/

### 2) Get STB files

You need ``stb_image.h`` and ``stb_image_write.h``.

Put them into the ``src/bindings/`` folder.

(probably will distribute them internally down the line)

### 3) Get GLFW3

Link: https://www.glfw.org/download.html

fixme: this part might be wrong.

Just drop the files in ``lib-static-ucrt/`` into the root of the Formine folder.

