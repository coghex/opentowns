# openTowns

openTowns is a recreation of the 2012 game towns witten in
lua and haskell using vulkan bindings.  simply
drop in the original data folder and you are good to go.
there are some differences between this and the original towns:
* towns was originally written in an ancient cuneiform known as
"java" and as such has many bugs, poor performance, and quirks. instead
all scripting is done in lua and all engine code is haskell. any xml
files are instanty converted to json
* the font is no longer an obsolete windows bitmap format now its
the cantarell ttf from the google website and the actual hinting from
the actual font is used, so it wont look the same.
* windowing is handled by GLFW instead of whatever java black magic was
implemented originally.  as such things like changing window size will
have slightly altered behavior.  the precise mouse position is used instead
of whatever system they used before.
* all game logic and input handling is threaded, the original game would stutter
* the original game was barely modable, since most of it was in java, modding
was through static xml files.  since this is all in lua, all functionality is
exposed from loading new textures, to forking new threads.
* game is balanced and new key shortcuts are added.
* all references to the original creators have been removed, i dont
beleive in intellectual property.

## Prerequisites

pre-requisites for building include [vulkan](https://vulkan.lunarg.org/sdk/home), [glfw](https://www.glfw.org/download.html),
and [freetype2](https://download.savannah.gnu.org/releases/freetype/).  all of these should be in
most package managers.  there are other libraries
such as libgmp3, libtinfo, glfw, and x11 stuff
that is also required.  works on windows, linux,
and mac; probably anything with GHC and vulkan.
if vulkan acts up and displays weird artifacts,
its because vulkan ships configured to override
your validation layers, run vkconfig and click
the box "fully controlled by vulkan application".
on windows the required LUNARG vulkan validation
layer may need to be downloaded from the sdk.

## Building

to build use `cabal new-build opentowns` to
download dependencies and compile.
`glslangValidator` must be in your path or
passed as a cabal argument as `cabal
--extra-lib-dirs=...\glslangValidator.exe
new-build opentowns`.

## Usage

use `./dist-newstyle/build/[arch]/ghc-[ver]/
abfa-[ver]/x/opentowns/opt/build/opentowns/opentowns +RTS
-s -M[x]m -N[n]` to run with x megabytes and
n cores.

performance is increased drastically by disabling
the development flag, but compilation takes
forever.

to create profiling files, use `cabal new-build
--enable-library-profiling --enable-profiling
abfa` to build, and `-prof -fprof-auto` in the
ghc-options of the cabal file.  run with flags
`+RTS -s -p -hy -M[x]m -N[n]`


## Development

the code is written with unicode, some extra
unicode is defined in `src/UPrelude`. which is
imported in every file.

feel free to open any issues for any reasons, pull
requests, forks, and stars are all appreciated.
most of the code is original, the files in
`/src/Vulk/` and `/src/Prog.hs`
are mostly from the [vulkan-api example](https://github.com/achirkin/vulkan/tree/master/vulkan-triangles).

the code structure is devided up into many parts
arbitrarily named:

* vulk - the graphics engine itself.
* prog - the continuation monad that
provides mutable state.
* luau - the lua interpreter and code
to generate game objects from the lua scripts.
* sign - a collection of threading functions to
pass data around between the various components.
