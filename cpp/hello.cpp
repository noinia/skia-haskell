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

// PDF stuff
#include "include/docs/SkPDFDocument.h"


// for the SDL stuff
#include "SDL.h" // the SDL lib itself
// and on the skia side
#include "include/gpu/ganesh/GrDirectContext.h"
#include "include/gpu/ganesh/gl/GrGLInterface.h"
#include "include/gpu/ganesh/SkSurfaceGanesh.h"
#include "include/gpu/ganesh/gl/GrGLDirectContext.h"

// openGL on mac os X
#include <OpenGL/gl.h>


// other includes?
#include "src/gpu/ganesh/gl/GrGLUtil.h"
#include "include/gpu/ganesh/gl/GrGLBackendSurface.h"
// #include "include/core/SkColorSpace.h"


////////////////////////////////////////////////////////////////////////////////

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



void mainSdL() {
  SDL_GLContext glContext = nullptr;
  uint32_t windowFlags = 0;

  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);

  windowFlags = SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE;

  static const int kStencilBits = 8;  // Skia needs 8 stencil bits
  SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 0);
  SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, kStencilBits);

  SDL_GL_SetAttribute(SDL_GL_ACCELERATED_VISUAL, 1);

  // If you want multisampling, uncomment the below lines and set a sample count
  static const int kMsaaSampleCount = 0; //4;

  /*
   * In a real application you might want to initialize more subsystems
   */
  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS) != 0) {
    std::cout << "ERROR initializing SDL";
    return;
  }
  int width = 640;
  int height = 480;
  SDL_Window* window = SDL_CreateWindow("SDL Window", SDL_WINDOWPOS_CENTERED,
                                        SDL_WINDOWPOS_CENTERED, width, height, windowFlags);


  // try and setup a GL context
  glContext = SDL_GL_CreateContext(window);
  int success =  SDL_GL_MakeCurrent(window, glContext);
  if (success != 0) {
    std::cout << "error sdl make current";
    return;
  }




  // You've already created your OpenGL context and bound it.
  sk_sp<const GrGLInterface> interface = nullptr;
  // Leaving interface as null makes Skia extract pointers to OpenGL functions for the current
  // context in a platform-specific way. Alternatively, you may create your own GrGLInterface
  // and initialize it however you like to attach to an alternate OpenGL implementation or
  // intercept Skia's OpenGL calls.
  sk_sp<GrDirectContext> context = GrDirectContexts::MakeGL(interface);
  SkImageInfo info = SkImageInfo:: MakeN32Premul(width, height);
  sk_sp<SkSurface> surface(SkSurfaces::RenderTarget(context.get(), skgpu::Budgeted::kNo, info));
  if (!surface) {
    SkDebugf("SkSurfaces::RenderTarget returned null\n");
    return;
  }



  // uint32_t windowFormat = SDL_GetWindowPixelFormat(window);
  // int contextType;
  // SDL_GL_GetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, &contextType);

  // int dw, dh;
  // SDL_GL_GetDrawableSize(window, &dw, &dh);

  // // glViewport(0, 0, dw, dh);
  // // glClearColor(1, 1, 1, 1);
  // // glClearStencil(0);
  // // glClear(GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

  // // setup GrContext
  // auto interface = GrGLMakeNativeInterface();

  // // setup contexts
  // sk_sp<GrDirectContext> grContext(GrDirectContexts::MakeGL(interface));
  // SkASSERT(grContext);


  // // Wrap the frame buffer object attached to the screen in a Skia render target so Skia can
  // // render to it
  // GrGLint buffer;
  // GR_GL_GetIntegerv(interface.get(), GR_GL_FRAMEBUFFER_BINDING, &buffer);
  // GrGLFramebufferInfo info;
  // info.fFBOID = (GrGLuint) buffer;
  // SkColorType colorType;

  // //SkDebugf("%s", SDL_GetPixelFormatName(windowFormat));
  // // TODO: the windowFormat is never any of these?
  // if (SDL_PIXELFORMAT_RGBA8888 == windowFormat) {
  //   info.fFormat = GR_GL_RGBA8;
  //   colorType = kRGBA_8888_SkColorType;
  // } else {
  //   colorType = kBGRA_8888_SkColorType;
  //   if (SDL_GL_CONTEXT_PROFILE_ES == contextType) {
  //     info.fFormat = GR_GL_BGRA8;
  //   } else {
  //     // We assume the internal format is RGBA8 on desktop GL
  //     info.fFormat = GR_GL_RGBA8;
  //   }
  // }

  // GrBackendRenderTarget target =
  //   GrBackendRenderTargets::MakeGL(width, height, kMsaaSampleCount, kStencilBits, info);

  //   // setup SkSurface
  //   // To use distance field text, use commented out SkSurfaceProps instead
  //   // SkSurfaceProps props(SkSurfaceProps::kUseDeviceIndependentFonts_Flag,
  //   //                      SkSurfaceProps::kUnknown_SkPixelGeometry);
  //   SkSurfaceProps props;

  //   sk_sp<SkSurface> surface(SkSurfaces::WrapBackendRenderTarget(grContext.get(), target,
  //                                                                   kBottomLeft_GrSurfaceOrigin,
  //                                                                   colorType, nullptr, &props));

    SkCanvas* canvas = surface->getCanvas();

    drawStuff(canvas);


    SDL_GL_SwapWindow(window);



  // //Destroy window
  // SDL_DestroyWindow(window);

  // //Quit SDL subsystems
  // SDL_Quit();

}



int main (int argc, char * const argv[]) {
  // hard coded example program parameters
  const char pngFilePath[] = "/tmp/skiaTestImage.png";
  const char svgFilePath[] = "/tmp/skiaTestImage.svg";
  const char pdfFilePath[] = "/tmp/skiaTestImage.pdf";

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


  mainSdL();

  ////////////////////////////////////////////////////////////////////////////////
  // render to PDF

  const char documentTitle[] = "title";
  constexpr SkSize pageSize{8.5f * 72, 11.0f * 72};
  // char* documentTitle =

  SkFILEWStream pdfStream(pdfFilePath);
  SkPDF::Metadata metadata;
  metadata.fTitle = documentTitle;
  metadata.fCreator = "Example WritePDF() Function";
  metadata.fCreation = {0, 2019, 1, 4, 31, 12, 34, 56};
  metadata.fModified = {0, 2019, 1, 4, 31, 12, 34, 56};
  auto pdfDocument = SkPDF::MakeDocument(&pdfStream, metadata);
  SkCanvas* pageCanvas = pdfDocument->beginPage(pageSize.width(),
                                                pageSize.height());
  drawStuff(pageCanvas);
  pdfDocument->endPage();

  pdfDocument->close();

  ////////////////////////////////////////////////////////////////////////////////

  return 0;
}
