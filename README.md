# fortran_thing
 I realized I never posted any fortran stuff to my github so here it is.

Also this is hand coded while looking at the glfw source code and docs so you'll have to excuse any mistakes.

Come join the Fortran Discord: https://discord.gg/tfn3M28ppP

### Install dependencies on fedora:
```
sudo dnf install gfortran openGL glfw-devel
```

This is using gfortran, gfortran is very buggy. This might be using some bugs which only come with gfortran. Test it in ifort for me to see where it is wrong.

# Ideas:
- hash table of shader uniforms
- verifier, you have to call verify_shader_uniforms(program_name, array_of_uniform_names)
- if any of them hit -1 stop the program, this means something has gone horribly wrong.


# Notes:

Allocatable - Will automatically deallocate once the variable goes out of scope. Do not manually deallocate cause that's dumb.
Pointer - Will NOT automatically deallocate.

in C:

``const char *`` would be interopped as ``character(kind = c_char), intent(in)``

``const char **`` would be interopped as ``character(len = *, kind = c_char), intent(in)``
