# fortran_thing
 I realized I never posted any fortran stuff to my github so here it is.

Also this is hand coded while looking at the glfw, opengl, stb, and luajit source code and docs so you'll have to excuse any mistakes.

This is written in Fortran 2003 up to 2018. Basically whatever my compiler (gfortran) allows.

Come join the Fortran Discord: https://discord.gg/tfn3M28ppP

And you can join my barren, empty, Discord: https://discord.gg/D95q7BHUE4

Here is what this thing currently looks like.

![Current progress.](https://raw.githubusercontent.com/jordan4ibanez/fortran_thing/master/screenshots/example_4.png)


scaling:

gui scaling

base the scale around 1920x1080 then use the current window size

1920/window_x or window_x/1920 to get a scalar value, do the same for the height

then take the smaller value, and this is the gui scale, this will stop things from going horribly wrong while keeping
a nice gui scaling!





-----

### Install dependencies on fedora:
```
sudo dnf install gfortran mesa-libGL-devel glfw-devel git
```

-----

### Install dependencies on linux mint 22+/ubuntu 24.04+
```
sudo apt install gfortran-14 libglfw3-dev libgl-dev libstb-dev libluajit-5.1-dev git
```
Then I would create a symbolic link to gfortran-14 in your ``.local/bin/`` directory via:
```
ln -s /usr/bin/gfortran-14 .local/bin/gfortran
```
(You might have to add ``.local/bin`` into your ``PATH``)

_Or_ you can just change the Makefile to use gfortran-14, but that's annoying.


### fpm requirement:

This is an fpm (Fortran Package Manager) project. There is a good reason for this.
fpm makes working with fortran extremely easy. Think cargo for rust, or deno for typescript.

To build this project properly, get fpm here:

https://github.com/fortran-lang/fpm

(You get the latest one in github releases.)

Then you can simply rename the executable to fpm and move it into the system bin location as so (if it's in your Downloads folder on Linux):

```
cd Downloads/

mv fpm-* fpm

chmod +x fpm

sudo mv fpm /usr/bin/
```

Now you should be able to run fpm anywhere in your system. :)

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

(Just use C types so it's easier to understand)

Allocatable - Will automatically deallocate once the variable goes out of scope. Do not manually deallocate cause that's dumb.
Pointer - Will NOT automatically deallocate.

NEVER, set ``this`` to ``value`` in a type method or IT WILL blow up!

in C:

(For C bindings)
``const char *`` would be interopped as ``character(kind = c_char), intent(in)``

This is because C does pointer arithmetic on the location we give it and we can use it to our advantage.
You must ensure that the string is null terminated. You can do this with ``string//achar(0)``

``const char **`` would be interopped as ``character(len = *, kind = c_char), intent(in)`` (if you only want 1)

This one, I am still unsure why we need the length component. But I'm sure someone will tell me eventually.




Back to fortran:

``character(len = :), allocatable`` is a dynamic length string in the heap.

BUT

That's not the same as ``character, dimension(:), allocatable`` because the former is scalar (1 value, [aka not an array]) and this one is an array of characters.

