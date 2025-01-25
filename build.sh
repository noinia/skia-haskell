#!/bin/sh

g++ -std=c++17 hello.cpp -framework CoreFoundation -framework \
  CoreGraphics -framework CoreText -framework CoreServices \
  -L/Users/frank/workspace/skia/out/Static -lskia  \
  -I/Users/frank/workspace/skia/ \
  -o hello \
  -I/opt/homebrew/opt/jpeg-turbo/include \
  -L/opt/homebrew/opt/jpeg-turbo/lib \
  -lz -ljpeg -lpng

  # -L/opt/homebrew/opt/webp/include/webp -llibwebp
  # -L/opt/homebrew/opt/webp/include/webp -llibwebp



# -I/Users/frank/workspace/skia/include/core \
  # -I/Users/frank/workspace/skia/include/config \
  # -I/Users/frank/workspace/skia/include/utils \
  # -I/Users/frank/workspace/skia/third_party/externals/sdl/include \
  # -I/Users/frank/workspace/skia/include/gpu \
  # -I/Users/frank/workspace/skia/src/gpu -o main

# g++ hello.cpp -o hello -L/Users/frank/workspace/skia/out/Static -lskia -I/Users/frank/workspace/skia/include/core/  -I/Users/frank/workspace/skia/include/config/  -I/Users/frank/workspace/skia/include/utils/  -I/Users/frank/workspace/skia/third_party/externals/sdl/include/  -I/Users/frank/workspace//skia/include/gpu/  -I/Users/frank/workspace//skia/src/gpu/
