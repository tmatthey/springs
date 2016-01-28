#include <string.h>  
#include <stdlib.h>
#include <stdio.h>  
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <math.h>

#include "global.h"
#include "display.h"
#include "fortrancall.h"

#define TIME_24 86400

void usage(char* name);
void parseCmdLine(int argc, char* argv[], char *in, char *out, int* savepic, BOOL* flagTrace, int* width, int* height);

extern void fmain_(INTP nstop, INTP n, DBLP t1, DBLP xi, DBLP xf, DBLP yi, DBLP yf, DBLP rx, DBLP ry, DBLP th, DBLP tag, INTP col, char* infile);

int main(int argc, char* argv[])
{
  char in[80];
  char out[80];
  
  XEvent report;
  KeySym key;
  BOOL flagWait = FALSE;
  BOOL flagTrace = FALSE;
  int savepic;
  int width = DEFAULT_WIDTH;
  int height = DEFAULT_HEIGHT;
  int nstop = -1;

  OutputWindow *window = NULL;

  int i;

  /* Data for springs.f */
  double* rx;
  double* ry;
  double* th;
  double* tag;
  int* col;
  double t1,xi,xf,yi,yf;
  int n;

  parseCmdLine(argc, argv, in, out, &savepic, &flagTrace, &width, &height);

  rx  = calloc(NMAX,sizeof(double));
  ry  = calloc(NMAX,sizeof(double));
  th  = calloc(NMAX,sizeof(double));
  tag = calloc(NMAX,sizeof(double));
  col = calloc(NMAX,sizeof(int));

  i = 0; 
  while (1){

    if (((i <= nstop) && (flagWait == FALSE)) || (window == NULL)) {
      fmain_(&nstop, &n,&t1, &xi, &xf, &yi, &yf, rx, ry, th, tag, col, in);
      if ((window == NULL) && ((window = openDisplay(xi,xf,yi,yf,width,height,TRUE,savepic,argc,argv,in, out)) == NULL))
        exit(-1);

      window->user = i;
      writeDisplay(window,rx,ry,th,tag,col,n);

      if (i == nstop)
	fprintf(stderr," Done.\n");

      i++;
    }

    if (flagTrace == TRUE)
      flagWait = TRUE;

    while ((XEventsQueued(window->display,QueuedAfterReading) > 0)||(i > nstop)||(flagWait == TRUE)){
      report.type = 0;
      XNextEvent(window->display, &report);

      switch (report.type){
        case Expose: 
          while (XCheckTypedEvent(window->display, Expose, &report));
          break;
          
        case ConfigureNotify:                /* Window has been resized */
          fprintf(stderr," Windowsize : %d, %d \n",report.xconfigure.width,
                                                report.xconfigure.height);
          break;

        case VisibilityNotify:
	  refreshDisplay(window);
	  break;

        case ButtonPress:
          break;

        case KeyPress:  /* Check the keys */
          key = XKeycodeToKeysym(window->display, report.xkey.keycode,0);
          switch (key){
          case XK_space:
            flagWait = (flagWait == TRUE) ? FALSE : TRUE; 
            break;
          case XK_t:
            if (flagTrace == FALSE)
	      flagTrace = TRUE;
	    else {
	      flagTrace = FALSE;
	      flagWait = FALSE;
	    }
            break;
          case XK_w:
            window->savepic |= SAVE_ONCE;
	    if (i >= nstop){
	      writeDisplay(window,rx,ry,th,tag,col,n);
	    }
            break;
          case XK_r:
            refreshDisplay(window);
	    break;
          case XK_q: /* exit gracefully */
            free(rx);
            free(ry);
            free(th);
            free(tag);
            free(col);
            closeDisplay(window);      
            exit(1);
          default: /* Throw away all other keys */
            break;
          }
          break;

        default:  /* Throw away all other events */
          break;
      }
    }
  }
}

void parseCmdLine(int argc, char* argv[], char* in, char *out, int* savepic, BOOL* flagTrace, int* width, int* height)
{
  int cur = 1;

  *savepic = 0;
  *flagTrace = FALSE;

  if ((argc == 2 && !strcmp(argv[1], "-h")) || argc>10 || argc < 2) {
    usage(argv[0]);
    exit(0);
  }
  
  
  while (cur<argc && argv[cur][0]=='-') {

    if (!strcmp(argv[cur],"-width")) {
      *width = atoi(argv[++cur]);
      cur++;
      continue;
    }

    if (!strcmp(argv[cur],"-ps")){
      *savepic |= SAVE_AS_PS;
      cur++;
      continue;
    }

    if (!strcmp(argv[cur],"-trace")){
      *flagTrace = TRUE;
      cur++;
      continue;
    }

    if (!strcmp(argv[cur],"-height")) {
      *height = atoi(argv[++cur]);
      cur++;
      continue;
    }
    
    if (!strcmp(argv[cur],"-pbm")){
      *savepic |= SAVE_AS_PBM;
      cur++;
      continue;
    }

    if (!strcmp(argv[cur],"-ppm")){
      *savepic |= SAVE_AS_PPM;
      cur++;
      continue;
    }

    if (!strcmp(argv[cur],"-all")){
      *savepic |= SAVE_ALLWAYS;
      cur++;
      continue;
    }

    break;

  }

  if ((*savepic == SAVE_ALLWAYS)|| (*savepic == 0L))
    *savepic |= SAVE_AS_PPM;
  
  in[0]  = 0;
  out[0] = 0;

  if (cur < argc) 
    strcpy(in,argv[cur++]);
  if (cur < argc)
    strcpy(out,argv[cur]);


}

/*_________________________________________________________*/

void usage(char* name)
{
    fprintf(stderr," Usage: %s [-width n] [-height n] [-trace] [-pbm] [-ps] [-all] [in-file] [out-file]\n",name);
    fprintf(stderr,"  where:\n");
    fprintf(stderr,"   -width n  : (optional) width of the window \n");
    fprintf(stderr,"   -height n : (optional) height of the window\n");
    fprintf(stderr,"   -pbm      : (optional) saves the pictures as PBM\n");
    fprintf(stderr,"   -ppm      : (optional, default) saves the pictures as PPM\n");
    fprintf(stderr,"   -ps       : (optional) saves the pictures as PS\n");
    fprintf(stderr,"   -all      : (optional) saves every pictures\n");
    fprintf(stderr,"   -trace    : (optional) stops after each step\n");
    fprintf(stderr,"  keys:\n");
    fprintf(stderr,"   q     : quit %s\n",name);
    fprintf(stderr,"   SPACE : stop/start\n");
    fprintf(stderr,"   t     : trace on/off\n");
    fprintf(stderr,"   w     : screendump of the actuall screen\n");
}








