#include <iostream>
#include "include/core/SkSurface.h"
#include "include/core/SkPath.h"
#include "include/core/SkCanvas.h"
#include "include/core/SkData.h"
#include "include/core/SkImage.h"
#include "include/core/SkStream.h"

#include "include/encode/SkPngEncoder.h"




// #include "include/svg/SkSVGCanvas.h"
// #include "xml/SkXMLWriter.h"

int main (int argc, char * const argv[]) {
  // hard coded example program parameters
  const char filePath[] = "/tmp/skiaTestImage.png";
  int width = 256;
  int height = 256;

  // create canvas to draw on
  sk_sp<SkSurface> rasterSurface = SkSurfaces::Raster(SkImageInfo::MakeN32Premul(width, height));
  SkCanvas* rasterCanvas = rasterSurface->getCanvas();


  draw(rasterCanvas);
  sk_sp<SkImage> img(rasterSurface->makeImageSnapshot());
  if (!img) { return; }
  sk_sp<SkData> png = SkPngEncoder::Encode(nullptr, img, {});
  if (!png) { return; }
  SkFILEWStream out(path);
  (void)out.write(png->data(), png->size());


  sk_sp<SkSurface> rasterSurface = SkSurfaces::Raster(SkImageInfo::MakeN32Premul(width, height));
  SkCanvas* canvas = rasterSurface->getCanvas();


  // SkFILEWStream svgStream(filePath);
  // std::unique_ptr<SkXMLWriter> xmlWriter(new SkXMLStreamWriter(&svgStream));
  // SkRect bounds = SkRect::MakeIWH(width, height);
  // std::unique_ptr<SkCanvas> canvas = SkSVGCanvas::Make(bounds, xmlWriter.get());


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

  // make a PNG encoded image using the canvas
  sk_sp<SkImage> img(rasterSurface->makeImageSnapshot());
  if (!img) {
    std::cout << "error img";
    return 1;
  }

  sk_sp<SkData> png = SkPngEncoder::Encode(nullptr, img, {});
  if (!png) {
    std::cout << "error png";
    return 1;
  }

  // write the data to the file specified by filePath
  std::cout << "writing file";
  SkFILEWStream out(filePath);
  (void)out.write(png->data(), png->size());

  return 0;
}
