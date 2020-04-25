# Haskell Raytracer

A simple raytracer written in Haskell.

## Compiling

Simple compile Main.hs, e.g `$ ghc Main.hs -o main`

## Usage

Currently the only way to modify the parameters (e.g which objects exist, image resolution, how many times to calculate light bounces) is to modify the code and recompile.

The program will output the rendered image to stdout in PPM format which can then be piped to a file, e.g `$ ./main > image.ppm`
