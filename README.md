# Formine
 A voxel game written in Fortran.

## Why is this called Formine?

I had a hard time naming this project. After much thought and research, I had an inspiration from the language creator himself, [John Backus](https://en.wikipedia.org/wiki/John_Backus). You can see this [here](https://youtu.be/KohboWwrsXg&t=471).

Fortran mining game. Formine.

Formine translates to **molds** in Italian. The type of molds you use to form things like metal casting and creating clay products.

I think this is quite fitting because Formine can not only be molded to how you envision it through the mod api, but it can also be molded with help from others due to it's open source nature.


## General notes

Also this is hand coded while looking at the glfw, opengl, stb, and luajit source code and docs so you'll have to excuse any mistakes.

This is written in Fortran 2003 up to 2018. Basically whatever my compiler (gfortran) allows.

If you like what I do, and would like to support me: [My Patreon](https://www.patreon.com/jordan4ibanez)

Come join the Fortran Discord: https://discord.gg/tfn3M28ppP

My Discord: https://discord.gg/D95q7BHUE4

Here is what this thing currently looks like.

![Current progress.](https://raw.githubusercontent.com/jordan4ibanez/fortran_thing/master/screenshots/example_8.png)


## Credits:

### JOML

Link: https://github.com/JOML-CI/JOML

License: MIT

Usage: Translated mat4f methods into fglm.

### (My) fast_pack

Link: https://github.com/jordan4ibanez/fast_pack

License: MIT

Usage: Translated from D to Fortran.


### Isabella II

Link: https://content.minetest.net/packages/zayuim/isabellaii/

License: CC-BY-3.0

Usage: The texture pack of the game.


-----

### fpm requirement (Linux):

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

### Install dependencies on fedora:
```
sudo dnf install gfortran mesa-libGL-devel glfw-devel git
```

-----

### Install dependencies on linux mint 22+/ubuntu 24.04+
```
sudo apt install make gfortran-14 libglfw3-dev libgl-dev libstb-dev libluajit-5.1-dev git
```
Then I would create a symbolic link to gfortran-14 in your ``.local/bin/`` directory via:
```
ln -s /usr/bin/gfortran-14 .local/bin/gfortran
```
(You might have to add ``.local/bin`` into your ``PATH``)

_Or_ you can just change the Makefile to use gfortran-14, but that's annoying.

-----

### Install dependencies on Mac OS

**Note:** this was tested on a 2014 mac mini running Mac OS 12 Monterey. If this breaks, let me know.

You're going to need MacPorts and Homebrew. This was tested in Mac OS Monterey.

Well first, you're going to need to edit your ``.zshrc`` to add a non-standard path.

Add this to ``.zshrc``:

```
export LIBRARY_PATH=$LIBRARY_PATH:/usr/local/lib
```

Mac Ports:
```
sudo port install fpm stb
```

Homebrew
```
brew install gfortran glfw
```

**You must switch** the ``link`` flags in the [fpm.toml](https://github.com/jordan4ibanez/Formine/blob/master/fpm.toml) to the mac one!

Then you simply run make.
```
make
```

-----

### Install dependencies on Windows 10/11 using MSYS2

You need to be sure you open MSYS2 UCRT64 or else it will not work.

```
pacman -S base-devel mingw-w64-ucrt-x86_64-fpm mingw-w64-ucrt-x86_64-gcc-fortran git mingw-w64-ucrt-x86_64-glfw mingw-w64-ucrt-x86_64-luajit mingw-w64-ucrt-x86_64-stb
```

**You must switch** the ``link`` flags in the [fpm.toml](https://github.com/jordan4ibanez/Formine/blob/master/fpm.toml) to the windows one!

**Make sure you git clone the project into the root directory of MSYS2 or else this will not work!**

```
cd /

git clone https://github.com/jordan4ibanez/Formine

cd Formine/

make windows
```

-----

### Compiler note:
This is using gfortran, gfortran is very buggy. This might be using some bugs which only come with gfortran. Test it in ifort for me to see where it is wrong.
Note: this uses fortran 14+, if you use this in 13.2 expect it to blow up trying to compile.

-----

### The Formine Logo:
(I am not an artist)

![Truly beautiful](https://raw.githubusercontent.com/jordan4ibanez/Formine/refs/heads/master/textures/formine_logo.png)