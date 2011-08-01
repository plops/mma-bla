#include <GL/glfw.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <sys/time.h>
#include <unistd.h>
#include <sys/fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#define NAN __builtin_nan("")

#define len(x) (sizeof(x)/sizeof(x[0]))

#include "digit.h"

enum { CMDLEN=128,
       CIRCBUFLEN=1000000,
       CIRCBUFNUMELEMS=CIRCBUFLEN-1,
       MAXARGS=3,
       DOCSTRINGLEN=200,
};

char *mma_fifo_fn="/dev/shm/mma-fifo";
int mma_fifo_fd;

int frame_count=0; // increments whenever a new frame is shown

// http://en.wikipedia.org/wiki/Circular_buffer
char *circbuf[CIRCBUFLEN];
// circular buffer contains pointer to commands
// there always must be an empty element between reader and writer
// text has to be freed after it is popped
int circwrite=0, // points where the next element will be written
  circread=0, // points where the next element will be read from
  circsize=CIRCBUFNUMELEMS;
void
circ_init()
{
  circwrite=0;
  circread=0;
  circsize=CIRCBUFNUMELEMS;
}

int emptyp(){ return circwrite==circread;}

int fullp(){ return ((circwrite+1)%CIRCBUFLEN)==circread;}

// increase but keep integer within 0..CIRCBUFLEN-1
int
inc(int *pos)
{
  int v=*pos;
  v++;
  v%=CIRCBUFLEN;
  (*pos)=v;
  return v;
}

int
push(char*s) // write the pointer s into the circular buffer
{
  if(fullp()){
    printf("error circbuffer is full\n");
    fflush(stdout);
  }
  circbuf[circwrite]=s;
  return inc(&circwrite);
}

char*
pop() // obtain next pointer from circular buffer
{
  if(emptyp()){
    //intf("error buffer is empty\n");
    return 0;
  }

  char*ret=circbuf[circread];
  printf("cread=%d cmd=%s\n",circread,ret);
  fflush(stdout);
  inc(&circread);

  return ret;
}


/*
line 0 0 100 100
line 10 3 100 100
line 42 23 100 100
swap
 */

int running=GL_TRUE;

// Draw a vertical line that moves to a different vertical position in
// every frame. This is useful for verifying the synchronization of
// the drawing function, screen and camera exposure.
int show_calibration_stripes=1;
// only when run_queue=1 the queue will start running
int run_queue=0;
int queue_images_displayed=0;

double
quit(double*ignore) 
{
  (void)ignore;
  running=GL_FALSE;
  return 0.0;
}

// this function can only be defined after cmd[]
double help(double*); 

double
qline(double*v)
{
  char*cmd=malloc(CMDLEN);
  snprintf(cmd,CMDLEN,"line %g %g %g %g",
	  v[0],v[1],v[2],v[3]);
  push(cmd);
  return 0.0;
}

double
line(double*v)
{
  glBegin(GL_LINES);
  glVertex2d(v[0],v[1]);
  glVertex2d(v[2],v[3]);
  glEnd();
  return 0.0;
}

double
qswap(double*v)
{
  char*cmd=malloc(CMDLEN);
  snprintf(cmd,CMDLEN,"swap");
  push(cmd);
  return 0.0;
}

double
swap(double*v)
{
  glfwSwapBuffers();
  return 0.0;
}


double
qclear(double*v)
{
  char*cmd=malloc(CMDLEN);
  snprintf(cmd,CMDLEN,"clear");
  push(cmd);
  return 0.0;
}

double
clear(double*v)
{
  glClear(GL_COLOR_BUFFER_BIT);
  return 0.0;
}
double
qdisk(double*v)
{
  char*cmd=malloc(CMDLEN);
  snprintf(cmd,CMDLEN,"disk %g %g %g",
	   v[0], v[1], v[2]);
  push(cmd);
  return 0.0;
}

double
disk(double*v)
{
  glPushMatrix();
  glTranslated(v[0],v[1],0);
  glScaled(v[2],v[2],v[2]);
  int i;
  glBegin(GL_TRIANGLE_FAN);
  glVertex2d(0,0);
  enum{NDISK=13};
  glVertex2d(0,1);
  for(i=1;i<NDISK;i++){
    double arg=i*2*M_PI/NDISK;
    glVertex2d(sin(arg),cos(arg));
  }
  glVertex2d(0,1);
  glEnd();
  glPopMatrix();
  return 0.0;
}

double
qnumber(double*v)
{
  char*cmd=malloc(CMDLEN);
  snprintf(cmd,CMDLEN,"number %d",(int)v[0]);
  push(cmd);
  return 0.0;
}

double
number(double*v)
{
  glPushMatrix();
  glTranslated(0,3*27,0);
  int n=(int)v[0];
  draw_number(n);
  glPopMatrix();
  return 1.0*n;
}

double
toggle_stripes(double*v){
  show_calibration_stripes=(int)v[0];
  return 1.0*show_calibration_stripes;
}

double
toggle_queue(double*v){
  run_queue=(int)v[0];
  queue_images_displayed=0;
  return 1.0*run_queue;
}

char mma_fn[200];
int do_mma_notify=0;

double
toggle_notify_mma(double*v){
  int pid=(int)v[0];
  do_mma_notify=0;
  if(pid){
    do_mma_notify=1;
    snprintf(mma_fn,200,"/proc/%d/fd/0",pid);
    printf("mma will be notified\n"); 
  } else {
    printf("mma won't be notified\n");
  }
  return 1.0*do_mma_notify;
}
double
qgrating_disk(double*v)
{
  char*cmd=malloc(CMDLEN);
  snprintf(cmd,CMDLEN,"grating-disk %g %g %g %g %g %g",
	   v[0], v[1], v[2]);
  push(cmd);
  return 0.0;
}

double
grating_disk(double*v)
{
  double 
    center_x=v[0],
    center_y=v[1],
    radius=v[2];
  int 
    phase=(int)v[3],
    phases=(int)v[4],
    line_width=(int)v[5];
  glPushMatrix();
  glLoadIdentity(); // we need pixel perfect drawing
  glBegin(GL_LINES);
  int xx;
  for(xx=(int)round(center_x-radius);
      xx<=(int)round(center_x+radius);
      xx+=phases*line_width){
    int x,xxx=xx+phase*line_width;
    for(x=xxx;x<xxx+line_width
  }

  glEnd();
  glPopMatrix();
}

// array that contains all functions that can be called from text interface
struct{ 
  char name[CMDLEN];
  int args, // number of float arguments
    queued; // is pushed into circular buffer
  double (*fptr)(double*);
  char docstring[DOCSTRINGLEN];
}cmd[]={{"help",0,0,help,"exit main program"},
       	{"quit",0,0,quit,"list all possible commands"},
	{"qline",4,1,qline,"draw a line x0 y0 x1 y1"},
	{"line",4,0,line,"immediately draw a line"},
	{"qdisk",3,1,qdisk,"draw a disk x y r"},
	{"disk",3,0,disk,"immediately draw a disk"},
	{"swap",0,0,swap,"immediate swap-buffers"},
	{"qswap",0,1,qswap,"initiate swap-buffers"},
	{"number",1,0,number,"immediately write number"},
	{"qnumber",1,1,qnumber,"queue write number"},
	{"clear",0,0,clear,"immediately clear screen"},
	{"qclear",0,1,qclear,"clear screen"},
	{"toggle-stripes",1,0,toggle_stripes,"toggle display of calibration stripes"},
	{"toggle-queue",1,0,toggle_queue,"toggle displaying things from the queue"},
	{"toggle-notify-mma",1,0,toggle_notify_mma,"if pid!=0 send stop start to mma process"},
};


// print synopsis of each possible command
double
help(double*ignore)
{
  (void)ignore;
  unsigned int i;
  printf("\nThis is a very simple parser. Each command is defined by one word\n"
	 "followed by some single float parameters.\n"
	 "The first column of this table is 'q', when the command will be queued\n"
	 "for later frame synchronized execution\n"
	 "cmd 'number of parameters' .. Description\n\n");
  for(i=0;i<len(cmd);i++)
    printf("%c %s %d .. %s\n",(cmd[i].queued==1)?'q':' ',
	   cmd[i].name,cmd[i].args,cmd[i].docstring);
  fflush(stdout);
  return 0.0;
}

// find the index of string s within cmd[]
int
lookup(char*s) 
{
  unsigned int i;
  int n=strlen(s);
  if(s[n-1]=='\n') // ignore trailing return
    n--;
  for(i=0;i<len(cmd);i++)
    if(0==strncmp(s,cmd[i].name,n))
      return i;
  return -1;
}

// is character c a character that would occur in a float?
int isfloatchar(int c) 
{
  if(c=='+'||c=='-'||c=='.'||isdigit(c))
    return 1;
  return 0;
}

// Parse the first token of a line as a name name
// and find this name within cmd[]. Return the index to cmd[] 
int
parse_name(char*tok) 
{
  int fun_index=-1;
  if(tok){
    if(isalpha(tok[0])){
      fun_index=lookup(tok);
    }else{
      printf("parse_name error, expected function name instead of %s\n", tok);
      fflush(stdout);
      return -1;
    }
  }else{
    printf("parse_name error, expected some function name but got nothing\n");
    fflush(stdout);
    return -1;
  }
  return fun_index;
}


// Split a line into a space separated tokens. THe first token should
// be the name of a function, the following tokens are parsed as float
// parameters
double
parse_line(char*line)
{
  char *search=" ",*tok;
  if(!line)
    return NAN;
  tok=strtok(line,search);

  int fun_index=parse_name(tok);
  if(fun_index<0)
    return NAN;

  int arg_num=cmd[fun_index].args;
  int i;
  double args[MAXARGS];
  for(i=0;i<arg_num;i++){
    tok=strtok(0,search);
    if(!tok){
      printf("error, expected an argument but got 0\n");
      fflush(stdout);
      return NAN;
    }
    if(isfloatchar(tok[0])){
        char*endptr;
        double d=strtod(tok,&endptr);
        if(endptr==tok){
          printf("error, couldn't parse double\n");
	  fflush(stdout);
	  return NAN;
        }else
          args[i]=d;
    }else{
      printf("error, expected digit or .+- but found %c\n",tok[0]);
      fflush(stdout);
      return NAN;
    }   
  }
  return cmd[fun_index].fptr(args);
}

// Keyboard callback. Exit program when escape key has been pressed.
void GLFWCALL
keyhandler(int key,int action)
{
  if(action!=GLFW_PRESS)
    return;
  if(key==GLFW_KEY_ESC)
    running=GL_FALSE;
  return;
}

// OpenGL Modelview Matrix
float m[4*4];

// Initialized Modelview matrix to do the affine transform from Camera
// coordinates into LCoS coordinates.
void
init_matrix()
{
  float s=.8283338739,
    sx=s,sy=-s,phi=-3.1017227,
    cp=cos(phi),sp=sin(phi),
    tx=608.43307,
    ty=168.91883;
  m[0]=sx*cp;
  m[1]=-1*sx*sp;
  m[2]=0;
  m[3]=0;
  
  m[4]=sy*sp;
  m[5]=sy*cp;
  m[6]=0.;
  m[7]=0.;

  m[8]=0;
  m[9]=0;
  m[10]=1;
  m[11]=0;
  
  m[12]=tx;
  m[13]=ty;
  m[14]=0;
  m[15]=1;
}

int
check_stdin()
{
  fd_set rfds;
  struct timeval tv;
  
  // Watch stdin (fd 0) to see when it has input.
  FD_ZERO(&rfds);
  FD_SET(0,&rfds);

  // wait for 10 ms				
  tv.tv_sec=0;
  tv.tv_usec=10000;
  
  int retval = select(1,&rfds,NULL,NULL,&tv);

  if(retval==-1)
    perror("select()");
  
  return retval;
}

int
replace_newline_0(char*lines,int*starts,int nstarts)
{
  // starts contains the start offsets of each line in lines. 
  // it has to be allocated by caller. the first value is always 0

  int i,n=strlen(lines),is=0;
  starts[is]=0;
  for(i=0;i<n;i++) // traverse string
    if(lines[i]=='\n'){
      lines[i]=0;
      is++;
      if(is>=nstarts){
	printf("starts can't contain all detected lines\n");
	return -1;
      }
      starts[is]=i+1;
    }
  return is; // return the number of detected lines
}

// write stop and start to the stdin of the mma control software
// this is used to synchronize mma and lcos
void
mma_sync()
{
  if(!do_mma_notify)
    return;
  FILE*f=fopen(mma_fn,"w");
  fprintf(f,"stop\n");
  fprintf(f,"start\n");
  fclose(f);
  printf("mma has been notified\n");
}


// Main program. Open a window. Continuously read commands from the
// command line interface and draw frames with 60Hz onto the
// screen. Commands for multiple frames can be cached within a ring
// buffer, so that a consistent frame rate can be maintained even if
// the control program doesn't respond within 16ms. The control
// program is written in Common Lisp and a 16ms time granularity cannot
// always be maintained, due to the time a garbage collection may
// take.
int
main(int argc,char**argv)
{
  
  // make sure frame rate update cycle is phase locked to vertical
  // refresh of screen. On Nvidia hardware this can be done by setting
  // the following environment variable.
  setenv("__GL_SYNC_TO_VBLANK","1",1); 

  //mkfifo(mma_fifo_fn,0664);
  //mma_fifo_fd=open(mma_fifo_fn,O_WRONLY);
  
  if(!glfwInit())
    exit(EXIT_FAILURE);
  int width=1280,height=1024;

  if(argc==3){
    width=atoi(argv[1]);
    height=atoi(argv[2]);
  }
   
  if(!glfwOpenWindow(width,height,8,8,8,
		     0,0,0,
		     GLFW_WINDOW
		     )){
    glfwTerminate();
    exit(EXIT_FAILURE);
  }
  printf("lcos started %dx%d\n",width,height);
  fflush(stdout);

  glfwSetWindowTitle("LCoS");
  //glfwSetWindowPos(-8,-31);

  // use glfw method to sync to vertical refresh
  glfwSwapInterval(1);

  glfwSetKeyCallback(keyhandler);
  init_matrix();
  
  glMatrixMode(GL_PROJECTION);
  glOrtho(0,1280,1024,0,-1,1);
  glMatrixMode(GL_MODELVIEW);

  struct timeval tv;    
  suseconds_t old_usec=0;
  time_t old_sec=0;

  
  while(running){
    while(check_stdin()>0){
      char*s=malloc(BUFSIZ);
      //char*line=fgets(s,CMDLEN,stdin);
      // make sure you only send one line on the fifo
      int n=read(0,s,BUFSIZ);
      if(n>=BUFSIZ)
	printf("read too much\n");
      if(n==0){
	printf("read empty\n");
	continue;
      }
      s[n]=0; 
      enum{NSTARTS=100};
      int starts[NSTARTS];
      int nlines=replace_newline_0(s,starts,NSTARTS);
      int i;
      for(i=0;i<nlines;i++){
	printf("i=%d '%s'\n",i,s+starts[i]);
	parse_line(s+starts[i]);
      }
      free(s);
    }

    frame_count++;
    gettimeofday(&tv,0);

    glClear(GL_COLOR_BUFFER_BIT);
    glLoadMatrixf(m);

    if(run_queue){ // run all commands which have been stored in the queue
      if(emptyp())
	 run_queue=0;
      while(!emptyp()){
	char*cmd=pop();
	
	if(0==strncmp(cmd,"swap",4)){
	  
	  printf("q swap frame-count=%d sec=%lu usec=%lu dt_ms=%3.3g\n",
		 frame_count,tv.tv_sec,tv.tv_usec,
		 (tv.tv_usec/1000.-old_usec/1000.)+(tv.tv_sec/1000.-old_sec/1000.));
	  
	  fflush(stdout);
	  free(cmd);
	  queue_images_displayed++;
	  goto nextframe;
	}
	parse_line(cmd);
	printf("q imgnr=%5d cread=%5d cwrite=%5d cmd=%s\n",
	       queue_images_displayed,circread,circwrite,cmd);
	fflush(stdout);
	free(cmd);
      }
    }
  nextframe:
    if(show_calibration_stripes){
      float v = 100+20*(frame_count%10);
      glRectf(v,0,v+2,400);
      draw_number(frame_count);
    }
    if(queue_images_displayed==1) // if we are at the first image synchronize if mma
	mma_sync();


    //glfwSleep(1./72);
    glfwSwapBuffers();
    old_usec=tv.tv_usec;
    old_sec=tv.tv_sec;	
  }
  
  glfwCloseWindow();

  glfwTerminate();
  printf("bye from lcos\n");

  close(mma_fifo_fd);
  return 0;
  //exit(EXIT_SUCCESS);
}
