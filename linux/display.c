#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xos.h>

#include "display.h"
#include "basic_bitmap.h"
#include "fortrancall.h"

#define min(a,b)               ( ((a) < (b)) ? (a) : (b) )
#define max(a,b)               ( ((a) > (b)) ? (a) : (b) )

/* Scales the the koordinates of the output */
#define SCALE_OF_OUTPUT 1.6
#define MAX_TO_DISPLAY 20000

#define num_cell_max 256

void get_GC(GC* gc, Window win, Display* display, int screen, XFontStruct *font_info);
unsigned long searchNearestColor(long r, long g, long b, Colormap cmap, Display* disp, int scr);
unsigned long allocColor(long r, long g, long b, Colormap cmap, Display* disp, int scr);

unsigned long allocColor(long r, long g, long b, Colormap cmap, Display* disp, int scr){
  XColor col;
  col.red = r;
  col.green = g;
  col.blue = b;
  if (XAllocColor(disp, cmap, &col) == False)
    return (searchNearestColor(r, g, b, cmap, disp, scr));
  return (col.pixel);
}

unsigned long searchNearestColor(long r, long g, long b, Colormap cmap, Display* disp, int scr){
  /* Findes the color of a given colormap which is nearest (r,g,b) */

  /*  const int num_cell_max 256; */
  XColor col[num_cell_max];
  unsigned long norm[num_cell_max];
  int num_cell = DisplayCells(disp, scr);
  int i,x;
  unsigned int min;
  unsigned int min_pixel = 0;
  if (num_cell > num_cell_max) 
    num_cell = num_cell_max;
  for (i=0; i<num_cell; i++)
    col[i].pixel = i;
  XQueryColors(disp, cmap, col, num_cell);
  r >>= 8;
  g >>= 8;
  b >>= 8;
  for (i=0; i<num_cell; i++){
    x = (r - (col[i].red >> 8));
    norm[i] = x*x;
    x = (g - (col[i].green >> 8));
    norm[i] += x*x;
    x = (b - (col[i].blue >> 8));
    norm[i] += x*x;
  }
  min = norm[0];
  for (i=0; i<num_cell; i++){
    if (norm[i] < min){
      min = norm[i];
      min_pixel = i;
    }
  }
  return min_pixel;
}

void get_GC(GC* gc, Window win, Display* display, int screen, XFontStruct *font_info)
{
  unsigned long valuemask = 0;
  XGCValues values;
  unsigned int line_width = 1;
  int line_style = LineSolid; /* LineOnOffDash; */
  int cap_style = CapRound;
  int join_style = JoinRound;
  int dash_offset = 0;
  static char dash_list[]={12,24};
  int list_length = 2;

  /* Get the graficcontext and set some attributes */
  *gc = XCreateGC(display, win, valuemask, &values);
  XSetFont(display, *gc, font_info->fid);
  XSetForeground(display, *gc, BlackPixel(display, screen));
  XSetLineAttributes(display, *gc, line_width, line_style, cap_style,join_style);
  XSetDashes(display, *gc, dash_offset, dash_list, list_length);
}

OutputWindow* openDisplay(REAL left, REAL right, REAL bottom, REAL top, int width, int height, BOOL flush, int savepic, int argc, char* argv[], char* in, char* out){
  Display* display;
  Window window;
  OutputWindow* ow;
  int screen;
  int width8;
  char *icon_name = "Springs";
  char *font_name = "9x15";
  Pixmap icon_pixmap;
  char name[80];
  unsigned long background,foreground;
  unsigned long valuemask;
  XSetWindowAttributes setwinattr;

  /* Before opening a window do some tests about the input */
  if (top <= bottom){
    printf("openDisplay: top <= bottom.\n");
    return NULL;
  }
  if (right <= left){
    printf("openDisplay: right <= left.\n");
    return NULL;
  }
  if (width <= 0){
    printf("openDisplay: width <= 0.\n");
    return NULL;
  }
  if (height <= 0){
    printf("openDisplay: height <= 0.\n");
    return NULL;
  }

  /* Correct the width */
  width8 = width & 0xFFFFFFF8;

  /* Open a display */
  display = XOpenDisplay(NULL);
  if (display == NULL){
    printf("openDisplay: Cannot open a XDisplay.\n");
    return NULL;
  }

  /* Default screen of the display */
  screen = DefaultScreen(display);

  /* Back- and foreground color */
  background = WhitePixel(display,screen);
  foreground = BlackPixel(display,screen);

  /* Open a window */
  window = XCreateSimpleWindow(display,RootWindow(display,screen),0,0,width8,height,4,
                               BlackPixel(display,screen),background);
  if (window == 0){
    XCloseDisplay(display);
    printf("Cannot open a XWindow.\n");
    return NULL;
  }

  /* Allocate memory for the datas of the outputwindow */
  ow = calloc(1,sizeof(OutputWindow));
  
  icon_pixmap = XCreateBitmapFromData(display, window, icon_bitmap_bits,
                                      icon_bitmap_width, icon_bitmap_height);

  XInstallColormap(display,DefaultColormap(display,screen));
  XSetWindowColormap(display,window,DefaultColormap(display,screen));

  /* Property of the window (p.417) */
  ow->size_hints.flags = PMinSize | PMaxSize ;
  ow->size_hints.x = 0;
  ow->size_hints.y = 0;
  ow->size_hints.width = width8;
  ow->size_hints.height = height;
  ow->size_hints.min_width = width8;
  ow->size_hints.min_height = height;
  ow->size_hints.max_width = width8;
  ow->size_hints.max_height = height;

  /* Title of the window */
  sprintf(name,"SPRINGS : %s",in);
  XSetStandardProperties(display, window, name, icon_name, icon_pixmap,
                         argv, argc, &ow->size_hints);

  /* Mask of desired events (p.292) */
  XSelectInput(display, window, KeyPressMask | ButtonPressMask | VisibilityChangeMask);

  /* Load a font */
  if ((ow->font_info = XLoadQueryFont(display, font_name)) == NULL){
    fprintf(stderr,"Cannot open %s font\n", font_name);
    return NULL;
  }
  
  /* Get the graficcontext */
  get_GC(&ow->gc, window, display, screen, ow->font_info);


  /* Set some windowattributes */
  valuemask = CWBackingStore;
  setwinattr.backing_store = Always;
  XChangeWindowAttributes(display, window, valuemask, &setwinattr);

  /* Map the window onto the screen */
  XMapWindow(display,window);
  XFlush(display);

  /* Allocate the backstorebuffer */
  ow->pixmap  = XCreatePixmap(display,window,width8,height,DefaultDepth(display,screen));

  /* Clear the backstorebuffer */
  XSetForeground(display,ow->gc,background);
  XFillRectangle(display,ow->pixmap,ow->gc,0,0,width8,height);
  XSetForeground(display,ow->gc,BlackPixel(display,screen));

  /* Set the parameters of the outputwindow */
  ow->display    = display;
  ow->window     = window;
  ow->screen     = screen;
  ow->background = background;
  ow->foreground = foreground;
  ow->left       = left;
  ow->right      = right;
  ow->bottom     = bottom;
  ow->top        = top;
  ow->width      = width8;
  ow->height     = height;
  ow->flush      = flush;
  ow->update     = TRUE;
  ow->init       = FALSE;
  ow->savepic    = savepic;
  ow->in         = in;
  ow->out        = out;
  ow->arcs       = NULL;
  ow->segments   = NULL;
  ow->a          = 1;
  ow->bx         = 0;
  ow->by         = 0;

  return ow;
}

void refreshDisplay(OutputWindow* ow){ 
  if (ow == NULL){
    return;
  }


}

void closeDisplay(OutputWindow* ow){

  if (ow == NULL)
    return;

  /* Free the backstorebuffer */
  XFreePixmap(ow->display,ow->pixmap);
  /* Free the allocated font */
  XUnloadFont(ow->display, ow->font_info->fid);
  /* Free the graficcontext */
  XFreeGC(ow->display, ow->gc);
  /* Close the display with all its windows */
  XCloseDisplay(ow->display);
  /* Free the memory for the X11 structs */
  free(ow->arcs);
  free(ow->segments);
  /* Free the allocated memory for the outputwindow */
  free(ow);
}

void flushDisplay(OutputWindow* ow){
  FILE* writefile = NULL;
  char name[200];
  char name2[200];
  char execline[500];
  unsigned long i,j,bit,k;
  XImage* image;
  XColor col[num_cell_max];
  int num_cell;

  /* Print the actual step */
  sprintf(name,"Step : %d",(int)ow->user);
  XSetForeground(ow->display,ow->gc,ow->foreground);
  XDrawString(ow->display, ow->pixmap, ow->gc, 20,20,name,strlen(name));

  /* Flush all events */
  XFlush(ow->display);

  /* Copy the backstorebuffer into the window and the buffer */
  XCopyArea(ow->display,ow->pixmap,ow->window,ow->gc,0,0,
            ow->width,ow->height,0,0);

  /* Screendump */
  if (((ow->savepic & SAVE_ALLWAYS) ==SAVE_ALLWAYS) || ((ow->savepic & SAVE_ONCE) ==SAVE_ONCE)){


    /* Save the actuall picture (backstorebuffer) as PBM */
    if ((ow->savepic & SAVE_AS_PBM) == SAVE_AS_PBM){
      if (ow->out[0] != 0)
        sprintf(name,"%s.%06d.pbm",ow->out,(int)ow->user);
      else
        sprintf(name,"Noname.%06d.pbm",(int)ow->user);
      /* Write out picture */
      writefile = fopen(name,"w");
      fprintf(writefile,"P4\n# %s\n%d %d\n",name,ow->width,ow->height);
      image = XGetImage(ow->display,ow->pixmap,0,0,ow->width,ow->height,AllPlanes,XYPixmap);
      for(i=0;i<ow->height;i++)
        for(j=0,bit=0,k=0;j<ow->width;j++){
          if (XGetPixel(image,j,i) != ow->background)
            k = (k << 1) | 1;
          else
            k = (k << 1);
          bit++;
          if (bit==8) {
            fputc(k,writefile);
            bit = k = 0;
          }
        }
      fclose(writefile);   
      XDestroyImage(image);
      fprintf(stderr,"%s written.\n",name);
    }

    /* Save the actuall picture (backstorebuffer) as PPM */
    if ((ow->savepic & SAVE_AS_PPM) == SAVE_AS_PPM){
      if (ow->out[0] != 0)
        sprintf(name,"%s.%06d.ppm",ow->out,(int)ow->user);
      else
        sprintf(name,"Noname.%06d.ppm",(int)ow->user);
      /* Write out picture */
      num_cell = DisplayCells(ow->display, ow->screen);
      if (num_cell > num_cell_max)
	num_cell = num_cell_max;
      for (i=0; i<num_cell; i++)
	col[i].pixel = i;
      XQueryColors(ow->display, DefaultColormap(ow->display,ow->screen), col, num_cell);
      writefile = fopen(name,"w");
      fprintf(writefile,"P6\n# %s\n%d %d\n255\n",name,ow->width,ow->height);
      image = XGetImage(ow->display,ow->pixmap,0,0,ow->width,ow->height,AllPlanes,XYPixmap);
      for(i=0;i<ow->height;i++)
        for(j=0;j<ow->width;j++){
          k = XGetPixel(image,j,i);
          /* fprintf(stdout,"(color (%i) at (%i,%i) out of range!)",k,i,j); */
          if (k >= num_cell)
            k = 0;
          fputc(col[k].red,writefile);
	  fputc(col[k].green,writefile);
	  fputc(col[k].blue,writefile);
        }
      fclose(writefile);   
      XDestroyImage(image);
      fprintf(stderr,"%s written.\n",name);
    }

    /* Save the actuall picture (backstorebuffer) as GIF */
    if ((ow->savepic & SAVE_AS_GIF) == SAVE_AS_GIF){
      if (ow->out[0] != 0){
        sprintf(name,"%s.%06d.gif",ow->out,(int)ow->user);
        sprintf(name2,"%s.%06d.ppm",ow->out,(int)ow->user);
      }
      else{
        sprintf(name,"Noname.%06d.gif",(int)ow->user);
        sprintf(name2,"Noname.%06d.ppm",(int)ow->user);
      }

      /* Write out picture as PPM first if not already done */
      if ((ow->savepic & SAVE_AS_PPM) != SAVE_AS_PPM){
        sprintf(name2,"springs.temp.%06d.ppm",(int)ow->user);
        num_cell = DisplayCells(ow->display, ow->screen);
        if (num_cell > num_cell_max)
	  num_cell = num_cell_max;
        for (i=0; i<num_cell; i++)
	  col[i].pixel = i;
        XQueryColors(ow->display, DefaultColormap(ow->display,ow->screen), col, num_cell);
          writefile = fopen(name2,"w");
        fprintf(writefile,"P6\n# %s\n%d %d\n255\n",name,ow->width,ow->height);
        image = XGetImage(ow->display,ow->pixmap,0,0,ow->width,ow->height,AllPlanes,XYPixmap);
        for(i=0;i<ow->height;i++)
          for(j=0;j<ow->width;j++){
            k = XGetPixel(image,j,i);
            /* fprintf(stdout,"(color (%i) at (%i,%i) out of range!)",k,i,j); */
            if (k >= num_cell)
              k = 0;
            fputc(col[k].red,writefile);
	    fputc(col[k].green,writefile);
	    fputc(col[k].blue,writefile);
          }
        fclose(writefile);   
        XDestroyImage(image);
        sprintf(execline,"ppmtogif %s > %s;rm -f %s",name2,name,name2);
      }
      else
        sprintf(execline,"ppmtogif %s > %s",name2,name);

      system(execline);
      fprintf(stderr,"%s written.\n",name);
    }

    /* Write the trailer for PostScript */
    if ((ow->savepic & SAVE_AS_PS) == SAVE_AS_PS){
      if (ow->out[0] != 0)
        sprintf(name,"%s.%06d.ps",ow->out,(int)ow->user);
      else
        sprintf(name,"Noname.%06d.ps",(int)ow->user);
      
      writefile = fopen(name,"a");
      fprintf(writefile,"showpage\n%% stop using temporary dictionary\nend\n\n%% restore original state\norigstate restore\n\n%%Trailer\n\n");
      fprintf(stderr,"%s written.\n",name);    
    }

    ow->savepic &= (~SAVE_ONCE);

  }

  /* Clear the backstorebuffer */
  XSetForeground(ow->display,ow->gc,ow->background);
  XFillRectangle(ow->display,ow->pixmap,ow->gc,0,0,ow->width,ow->height);
  XSetForeground(ow->display,ow->gc,ow->foreground);

  ow->update = TRUE;
}

void writeDisplay(OutputWindow* ow, DBLP xp, DBLP yp, DBLP tp, DBLP radius, INTP col, int n){
  int i,n1,n2,m,l,k;
  unsigned long color;
  BOOL flagBold;
  char name[200];
  FILE* writefile = NULL;
  
  double x1,y1,x2,y2,s;

  /* Init arrays, but only the first time */ 
  if (ow->init == FALSE){         
    ow->init = TRUE;

    /* Allocate memory for the X11 structs */
    if (ow->arcs == NULL){ 
      ow->arcs = calloc(NMAX,sizeof(XArc));
    }
    if (ow->segments == NULL){ 
      ow->segments = calloc(NMAX,sizeof(XSegment));
    }
    
    ow->a = min(0.5*ow->width/(ow->right - ow->left),0.5*ow->height/(ow->top - ow->bottom)) * SCALE_OF_OUTPUT;
    ow->bx = -ow->a * 0.5 * (ow->left   + ow->right) + 0.5 * ow->width;
    ow->by =  ow->a * 0.5 * (ow->bottom + ow->top)   + 0.5 * ow->height-1;
  }

  k = 0;
  m = 0;
  while(m < n) {
    /* Copy the results into the X11-structs */
    l = 0;
    for(i=0; i< n;i++){
      if (k == col[i]) {
        ow->arcs[l].width = radius[i]*2.0*ow->a;
        ow->arcs[l].height = radius[i]*2.0*ow->a;
        ow->arcs[l].angle1 = 0;
        ow->arcs[l].angle2 = 360*64;
        ow->arcs[l].x = ow->a*xp[i]+ow->bx;
        ow->arcs[l].y = -ow->a*yp[i]+ow->by;
        ow->segments[l].x1 = ow->arcs[l].x;
        ow->segments[l].y1 = ow->arcs[l].y;
        ow->segments[l].x2 = (REAL)ow->arcs[l].x+radius[i]*ow->a*cos(tp[i]);
        ow->segments[l].y2 = (REAL)ow->arcs[l].y-radius[i]*ow->a*sin(tp[i]);
        ow->arcs[l].x -= radius[i]*ow->a;
        ow->arcs[l].y -= radius[i]*ow->a;
        l++;
      }
    }

    switch (k){
        case 1: 
          color = allocColor(255<<8,0<<8,0<<8,DefaultColormap(ow->display,ow->screen),ow->display,ow->screen);
          break;

        case 2: 
          color = allocColor(0<<8,255<<8,0<<8,DefaultColormap(ow->display,ow->screen),ow->display,ow->screen);
          break;

        case 3: 
          color = allocColor(0<<8,0<<8,255<<8,DefaultColormap(ow->display,ow->screen),ow->display,ow->screen);
          break;

        default:
          color = BlackPixel(ow->display,ow->screen);
          break;

	}

    XSetForeground(ow->display,ow->gc,color);

    /* Draw into the backbuffer */
    n1 = l % MAX_TO_DISPLAY;
    n2 = l / MAX_TO_DISPLAY;
    for(i=0;i <n2;i++){
      XDrawSegments(ow->display,ow->pixmap,ow->gc,ow->segments+i*MAX_TO_DISPLAY,MAX_TO_DISPLAY);
      XDrawArcs(ow->display,ow->pixmap,ow->gc,ow->arcs+i*MAX_TO_DISPLAY,MAX_TO_DISPLAY);
    }
    
    if (n1 > 0){
      XDrawSegments(ow->display,ow->pixmap,ow->gc,ow->segments+l-n1,n1);
      XDrawArcs(ow->display,ow->pixmap,ow->gc,ow->arcs+l-n1,n1);
    }
    m += l;
    k++;
  }

  if (((ow->savepic & SAVE_ALLWAYS) ==SAVE_ALLWAYS) || ((ow->savepic & SAVE_ONCE) ==SAVE_ONCE)){
    /* Save as PS */
    if ((ow->savepic & SAVE_AS_PS) == SAVE_AS_PS){
      if (ow->out[0] != 0)
	sprintf(name,"%s.%06d.ps",ow->out,(int)ow->user);
      else
	sprintf(name,"Noname.%06d.ps",(int)ow->user);

      /* Write out data */

      s = 600.0 / ow->width;
      flagBold = FALSE;

      if (ow->update == TRUE){
	writefile = fopen(name,"w");
        fprintf(stderr,"%s\n",name);
        /* The header */
	fprintf(writefile,"%%!PS-Adobe-2.0 EPSF-2.0\n");
	fprintf(writefile,"%%%%Title: %s\n",name);
	fprintf(writefile,"%%%%Creator: SPRINGS by Jan Petter Hansen, Thierry Matthey, 1997\n");
	fprintf(writefile,"%%%%Orientation: Portrait\n");
	fprintf(writefile,"%%%%BoundingBox: 0 0 %i %i\n",(int)(ow->width*s),(int)(ow->height*s)); 
	fprintf(writefile,"%%%%Pages: 1\n" );
	fprintf(writefile,"%%%%DocumentFonts:\n" );
	fprintf(writefile,"%%%%EndComments\n");
	fprintf(writefile,"%%%%EndProlog\n");
	fprintf(writefile,"%%%%Page: 1 1\n");
	fprintf(writefile,"\n");
	fprintf(writefile,"%% remember original state\n");
	fprintf(writefile,"/origstate save def\n");
	fprintf(writefile,"\n");
	fprintf(writefile,"%% build a temporary dictionary\n");
	fprintf(writefile,"20 dict begin\n");
	fprintf(writefile,"\n");
	fprintf(writefile,"%% some settings\n");
	fprintf(writefile,".5 setlinewidth\n");
	fprintf(writefile,"1 setlinejoin\n");
	fprintf(writefile,"1 setlinecap\n");
	fprintf(writefile,"\n");
	fprintf(writefile,"%%some definitions\n");
	fprintf(writefile,"/m /moveto load def                           %% moveto\n");
	fprintf(writefile,"/d { lineto currentpoint stroke moveto } def  %% draw line\n");
	fprintf(writefile,"/e { lineto currentpoint stroke } def         %% endpoint of line\n");
	fprintf(writefile,"/c { /matr exch def            %% store matrix\n");
	fprintf(writefile,"     gsave matr concat         %% set transformation\n");
	fprintf(writefile,"     arc                       %% draw circle\n");
	fprintf(writefile,"     matr matrix invertmatrix  %% create inverse matrix\n");
	fprintf(writefile,"     concat stroke             %% draw line in original context\n");
	fprintf(writefile,"     grestore } def   %% circle with transf\n");
	fprintf(writefile,"\n");
	fprintf(writefile,"%% define clipping path\n");
	fprintf(writefile,"newpath\n");
	fprintf(writefile,"0 0 m\n");
	fprintf(writefile,"0 %i lineto\n",(int)(ow->height*s));
	fprintf(writefile,"%i %i lineto\n",(int)(ow->width*s),(int)(ow->height*s));
	fprintf(writefile,"%i 0 lineto\n",(int)(ow->width*s));
	fprintf(writefile,"closepath clip\n");
	fprintf(writefile,"newpath\n");
	fprintf(writefile,"\n");
	fprintf(writefile,"%% Global Positioning\n");
	fprintf(writefile,"0 0 translate\n");
	fprintf(writefile,"1 1 scale\n");
	fprintf(writefile,"\n");
	fprintf(writefile,"%% define space for color conversions\n");
	fprintf(writefile,"/grays 72 string def  %% space for gray scale line\n");
	fprintf(writefile,"/npixls 0 def\n");
	fprintf(writefile,"/rgbindx 0 def\n");
	fprintf(writefile,"\n");
	fprintf(writefile,"%% define 'colorimage' if it isn't defined\n");
	fprintf(writefile,"/colorimage where   %% do we know about 'colorimage'?\n");
	fprintf(writefile,"  { pop }           %% yes: pop off the 'dict' returned\n");
	fprintf(writefile,"  {                 %% no:  define one\n");
	fprintf(writefile,"    /colortogray {  %% define an RGB->I function\n");
	fprintf(writefile,"      /rgbdata exch store    %% call input 'rgbdata'\n");
	fprintf(writefile,"      rgbdata length 3 idiv\n");
	fprintf(writefile,"      /npixls exch store\n");
	fprintf(writefile,"      /rgbindx 0 store\n");
	fprintf(writefile,"      0 1 npixls 1 sub {\n");
	fprintf(writefile,"        grays exch\n");
	fprintf(writefile,"        rgbdata rgbindx       get 20 mul    %% Red\n");
	fprintf(writefile,"        rgbdata rgbindx 1 add get 32 mul    %% Green\n");
	fprintf(writefile,"        rgbdata rgbindx 2 add get 12 mul    %% Blue\n");
	fprintf(writefile,"        add add 64 idiv      %% I = .5G + .31R + .18B\n");
	fprintf(writefile,"        put\n");
	fprintf(writefile,"        /rgbindx rgbindx 3 add store\n");
	fprintf(writefile,"      } for\n");
	fprintf(writefile,"      grays 0 npixls getinterval\n");
	fprintf(writefile,"    } bind def\n");
	fprintf(writefile,"\n");
	fprintf(writefile,"    %% Utility procedure for colorimage operator.\n");
	fprintf(writefile,"    %% This procedure takes two procedures off the\n");
	fprintf(writefile,"    %% stack and merges them into a single procedure.\n");
	fprintf(writefile,"\n");
	fprintf(writefile,"    /mergeprocs { %% def\n");
	fprintf(writefile,"      dup length\n");
	fprintf(writefile,"      3 -1 roll\n");
	fprintf(writefile,"      dup\n");
	fprintf(writefile,"      length\n");
	fprintf(writefile,"      dup\n");
	fprintf(writefile,"      5 1 roll\n");
	fprintf(writefile,"      3 -1 roll\n");
	fprintf(writefile,"      add\n");
	fprintf(writefile,"      array cvx\n");
	fprintf(writefile,"      dup\n");
	fprintf(writefile,"      3 -1 roll\n");
	fprintf(writefile,"      0 exch\n");
	fprintf(writefile,"      putinterval\n");
	fprintf(writefile,"      dup\n");
	fprintf(writefile,"      4 2 roll\n");
	fprintf(writefile,"      putinterval\n");
	fprintf(writefile,"    } bind def\n");
	fprintf(writefile,"\n");
	fprintf(writefile,"    /colorimage { %% def\n");
	fprintf(writefile,"      pop pop     %% remove 'false 3' operands\n");
	fprintf(writefile,"      {colortogray} mergeprocs\n");
	fprintf(writefile,"      image\n");
	fprintf(writefile,"    } bind def\n");
	fprintf(writefile,"  } ifelse          %% end of 'false' case\n");
	fprintf(writefile,"\n");
      }
      else
	writefile = fopen(name,"a");

      for(i=0; i< n;i++){
	x1 = ow->a*xp[i]+ow->bx;
	y1 = ow->height - (-ow->a*yp[i]+ow->by);
        x2 = x1 + radius[i]*ow->a*cos(tp[i]);
        y2 = y1 - radius[i]*ow->a*sin(tp[i]);
	if ((flagBold == FALSE) && (col[i] == 1)) {
	  fprintf(writefile,"1.0 setlinewidth\n");
	  flagBold = TRUE;
	}
	else if ((flagBold == TRUE) && (col[i] != 1)){
	  fprintf(writefile,".5 setlinewidth\n");
	  flagBold = FALSE;
        }
	fprintf(writefile,"%f %f %f 0 360 [1 0 0 1 0 0] c\n",x1*s,y1*s,radius[i]*ow->a*s);
        fprintf(writefile,"%f %f m\n",x1*s,y1*s);
        fprintf(writefile,"%f %f e\n",x2*s,y2*s);
      }

      fclose(writefile);
    }
  }


  ow->update = FALSE;
  if (ow->flush == TRUE)
    flushDisplay(ow);

}



























