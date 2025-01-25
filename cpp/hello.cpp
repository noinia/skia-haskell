#include <iostream>
#include "include/core/SkSurface.h"
#include "include/core/SkPath.h"
#include "include/core/SkCanvas.h"
#include "include/core/SkData.h"
#include "include/core/SkImage.h"
#include "include/core/SkStream.h"

// for the PNG writer
#include "include/encode/SkPngEncoder.h"

// for the svg example
#include "include/svg/SkSVGCanvas.h"
#include "src/xml/SkXMLWriter.h"



// Our drawing function
void drawStuff(SkCanvas* canvas) {
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
}





int main (int argc, char * const argv[]) {
  // hard coded example program parameters
  const char pngFilePath[] = "/tmp/skiaTestImage.png";
  const char svgFilePath[] = "/tmp/skiaTestImage.svg";
  const char pdfFilePath[] = "/tmp/skiaTestImage.svg";

  int width = 256;
  int height = 256;

  ////////////////////////////////////////////////////////////////////////////////
  // render to PNG example
  ////////////////////////////////////////////////////////////////////////////////

  // create canvas to draw on
  sk_sp<SkSurface> rasterSurface = SkSurfaces::Raster(SkImageInfo::MakeN32Premul(width, height));
  SkCanvas* canvas = rasterSurface->getCanvas();

  drawStuff(canvas);

  SkPixmap pixmap;
  rasterSurface->peekPixels(&pixmap);

  SkFILEWStream output(pngFilePath);
  if (!SkPngEncoder::Encode(&output, pixmap, {})) {
    std::cout << "PNG encoding failed.\n";
    return 1;
  }
  std::cout << "PNG success\n";

  ////////////////////////////////////////////////////////////////////////////////
  // render to SVG example
  ////////////////////////////////////////////////////////////////////////////////

  SkFILEWStream svgStream(svgFilePath);
  // std::unique_ptr<SkXMLWriter> xmlWriter(new SkXMLStreamWriter(&svgStream));

  SkRect bounds = SkRect::MakeIWH(width, height);
  std::unique_ptr<SkCanvas> svgCanvas = SkSVGCanvas::Make(bounds, &svgStream);

  drawStuff(svgCanvas.get());

  return 0;
}
