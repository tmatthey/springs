#ifndef _DISPLAY_H
#define _DISPLAY_H

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>

#include "global.h"
#include "fortrancall.h"

#define SAVE_ONCE    1<<0
#define SAVE_ALLWAYS 1<<1
#define SAVE_AS_RAS  1<<2
#define SAVE_AS_PBM  1<<3
#define SAVE_AS_PS   1<<4
#define SAVE_AS_PPM  1<<5
#define SAVE_AS_GIF  1<<6

#define DEFAULT_WIDTH 800
#define DEFAULT_HEIGHT 600

/* Structur for the datas for the output */
typedef struct {
  /* X11 */
  Display* display;
  Window window;
  Pixmap pixmap;  /* The drawbuffer */
  XSizeHints size_hints;
  GC gc;
  XFontStruct *font_info;
  int screen;

  /* Window size */
  REAL left,right;   /* Size of the input */
  REAL bottom,top;
  int width,height;  /* Size of the outputwindow */
  REAL a,bx,by;      /* Scalefactors */

  /* Internal data */
  BOOL flush;        /* If FALSE then dont flush after writeDisplay(); */
  BOOL update;    
  BOOL init;
  int  savepic;
  unsigned long background; /* Color of the background */
  unsigned long foreground; /* Color of the foreground */

  /* X11 Structs */
  XArc *arcs;
  XSegment *segments;

  char* out;
  char* in;
  unsigned long user; /* The actual step */
} OutputWindow; 

/* Opening a X11 window and creating an OutputWindow structur to save datas for output */
extern OutputWindow* openDisplay(REAL left, REAL right, REAL bottom, REAL top, int width, int height, BOOL flush, int savepic, int argc, char* argv[],char* in, char* out);

/* Closing the outputwindow and freeing the allocated memeory */
extern void closeDisplay(OutputWindow* ow);

/* Flushing the drawing from the backstorebuffer onto the visible window */
extern void flushDisplay(OutputWindow* ow);

/* Drawing the datas into the backstorebuffer */
extern void writeDisplay(OutputWindow* ow, DBLP xp, DBLP yp, DBLP tp, DBLP radius, INTP col, int n);

/* Refreshing the drawing */
extern void refreshDisplay(OutputWindow* ow);

#endif




















