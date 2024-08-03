# fortran_thing
 I realized I never posted any fortran stuff to my github so here it is.

Also this is hand coded while looking at the glfw source code and docs so you'll have to excuse any mistakes.

Come join the Fortran Discord: https://discord.gg/tfn3M28ppP

And you can join my barren, empty, Discord: https://discord.gg/D95q7BHUE4

Here is what this thing currently looks like.

![Current progress.](https://raw.githubusercontent.com/jordan4ibanez/fortran_thing/master/screenshots/example_3.png)

-----

### Install dependencies on fedora:
```
sudo dnf install gfortran mesa-libGL-devel glfw-devel
```

-----

### Install dependencies on linux mint 22+/ubuntu 24.04+
```
sudo apt install gfortran-14 libglfw3-dev libgl-dev libstb-dev
```
Then I would create a symbolic link to gfortran-14 in your ``.local/bin/`` directory via:
```
ln -s /usr/bin/gfortran-14 .local/bin/gfortran
```
(You might have to add ``.local/bin`` into your ``PATH``)

_Or_ you can just change the Makefile to use gfortran-14, but that's annoying.

-----

### Compiler note:
This is using gfortran, gfortran is very buggy. This might be using some bugs which only come with gfortran. Test it in ifort for me to see where it is wrong.
Note: this uses fortran 14+, if you use this in 13.2 expect it to blow up trying to compile.

# Ideas:
- hash table of shader uniforms
- verifier, you have to call verify_shader_uniforms(program_name, array_of_uniform_names)
- if any of them hit -1 stop the program, this means something has gone horribly wrong.


# Notes:

Implicit save: (Thanks to Verlio_H for notifying that this is a common pitfall)
If you initialize a variable in a type, it just becomes the default.
If you initialize a variable in a subroutine or a function, it implicitly gets the save attribute. This will maintain state across each call of the function.
  With the default initializer being it's initial value.

ways to define 32 bit floats:
real(4)
real(real32)
real(c_float)

ways to define 64 bit floats
real(8)
real(real64) 
real(c_double)

(maybe should still use iso_c_bindings wordings when writing C wrappers though)

going to need to replace the uint with logical(int32) https://www.ibm.com/docs/en/xl-c-aix/13.1.2?topic=fortran-corresponding-data-types

Allocatable - Will automatically deallocate once the variable goes out of scope. Do not manually deallocate cause that's dumb.
Pointer - Will NOT automatically deallocate.

NEVER, set ``this`` to ``value`` in a type method or IT WILL blow up!

in C:

(For C bindings)
``const char *`` would be interopped as ``character(kind = c_char), intent(in)``

``const char **`` would be interopped as ``character(len = *, kind = c_char), intent(in)``

Back to fortran:

``character(len = :), allocatable`` is a dynamic length string in the heap.
