#include <iostream>
#include "include/core/SkSurface.h"
#include "include/core/SkPath.h"
#include "include/core/SkCanvas.h"
#include "include/core/SkData.h"
#include "include/core/SkImage.h"
#include "include/core/SkStream.h"

#include "include/encode/SkPngEncoder.h"

// #include "include/svg/SkSVGCanvas.h"
// #include "SkXMLWriter.h"


// #include "include/svg/SkSVGCanvas.h"
// #include "xml/SkXMLWriter.h"

int main (int argc, char * const argv[]) {
  // hard coded example program parameters
  const char filePath[] = "/tmp/skiaTestImage.png";
  int width = 256;
  int height = 256;

  // create canvas to draw on
  sk_sp<SkSurface> rasterSurface = SkSurfaces::Raster(SkImageInfo::MakeN32Premul(width, height));
  SkCanvas* canvas = rasterSurface->getCanvas();


  ////////////////////////////////////////////////////////////////////////////////
  // Draw stuff
  ////////////////////////////////////////////////////////////////////////////////

  // creating a path to be drawn
  SkPath path;
  path.moveTo(10.0f, 10.0f);
  path.lineTo(100.0f, 0.0f);
  path.lineTo(100.0f, 100.0f);
  path.lineTo(0.0f, 100.0f);
  path.lineTo(50.0f, 50.0f);
  path.close();

  // creating a paint to draw with
  SkPaint p;
  p.setAntiAlias(true);

  // clear out which may be was drawn before and draw the path
  canvas->clear(SK_ColorWHITE);
  canvas->drawPath(path, p);

  ////////////////////////////////////////////////////////////////////////////////


  SkPixmap pixmap;
  rasterSurface->peekPixels(&pixmap);

  SkFILEWStream output(filePath);
  if (!SkPngEncoder::Encode(&output, pixmap, {})) {
    std::cout << "PNG encoding failed.\n";
    return 1;
  }
  std::cout << "success\n";
  return 0;
}
