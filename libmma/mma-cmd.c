// the parsing code isn't thread safe. I use strtok

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include "slm.h"

#define len(x) (sizeof(x)/sizeof(x[0]))
#define e(q) do{fprintf(stderr,"error in file %s:%d in function %s, while calling %s\n",__FILE__,__LINE__,__FUNCTION__,q);}while(0)
#define NAN __builtin_nan("")
enum{
  CMDLEN=100, // maximum length of command strings
  MAXARGS=3,  // maximum number of arguments for a function
  N=256, // size of MMA
  NN=N*N
};

unsigned short*buf;

// functions that can be called from text interface

double
status(double*ignore)
{
  (void)ignore;
  unsigned int stat,error;
  if(0!=SLM_ReadStatus(&stat,&error)){
    e("read-status");
    return NAN;
  }
  printf("status %d error %d\n",stat,error);
  return 0.0;
}


double
splat(double*args)
{
  int i=(int)args[0],j=(int)args[1],d=(int)args[2],x,y;
  for(x=0;x<NN;x++)
    buf[x]=90;
  for(y=-d;y<=d;y++)
    for(x=-d;x<=d;x++){
      int xx=x+i,yy=y+j;
      if((0<=xx) && (xx<N) && (0<=yy) && (yy<N))
	buf[xx+N*yy]=4095;
    }
  return 0.;
}

double
img(double*ignore)
{
  (void)ignore;
  if(0!=SLM_WriteMatrixData(1,3,buf,N*N)){
    printf("error upload-image\n");
    return NAN;
  }
  return 0.0;
}

double
set_cycle_time(double*args)
{
  double time_ms=args[0];
  if(0!=SLM_SetCycleTime(time_ms)){
    e("cycle time");
    return NAN;
  }
  return 0.0;
}

double
start(double*ignore)
{
  (void)ignore;
  if(0!=SLM_SetStartMMA()){
    e("start");
    return NAN;
  }
  return 0.0;
}

double
stop(double*ignore)
{
  (void)ignore;
  if(0!=SLM_SetStopMMA()){
    e("stop");
    return NAN;
  }
  return 0.0;
}

double
off(double*ignore)
{
  (void)ignore;
  if(0!=SLM_SetPowerOff()){
    e("power off");
    return NAN;
  }
  return 0.0;
}

double
on(double*ignore)
{
  (void)ignore;
  if(0!=SLM_SetPowerOn()){
    e("power on");
    return NAN;
  }
  return 0.0;
}

// array that contains all functions that can be called from text interface
struct{ 
  char name[CMDLEN];
  int args;
  double (*fptr)(double*);
}cmd[]={{"img",0,img},
	{"splat",3,splat},
	{"status",0,status},
	{"start",0,start},
	{"stop",0,stop},
	{"on",0,on},
	{"off",0,off},
	{"set_cycle_time",1,set_cycle_time},};



// code for parsing text interface
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

int isfloatchar(int c)
{
  if(c=='+'||c=='-'||c=='.'||isdigit(c))
    return 1;
  return 0;
}

int
parse_name(char*tok)
{
  int fun_index=-1;
  if(tok){
    if(isalpha(tok[0])){
      fun_index=lookup(tok);
      printf("+%s=%d+\n",tok,fun_index);
    }else{
      printf("error, expected function name\n");
      return -1;
    }
  }else{
    printf("error, expected some function name but got nothing");
    return -1;
  }
  return fun_index;
}

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
      printf("error, expected an argument");
      return NAN;
    }
    if(isfloatchar(tok[0])){
	char*endptr;
	double d=strtod(tok,&endptr);
	if(endptr==tok){
	  printf("error, couldn't parse double\n");
	  return NAN;
	}else
	  args[i]=d;
	  printf("%g\n",d);
    }else{
      printf("error, expected digit or .+- but found %c\n",tok[0]);
      return NAN;
    }   
  }
  return cmd[fun_index].fptr(args);
}



int
main()
{
  buf=malloc(N*N*2);
  int i;
  for(i=0;i<NN;i++)
    buf[i]=90;
  if(0!=SLM_RegisterBoard(0x0036344B00800803LL,
			  "192.168.0.2","255.255.255.0",
			  "0.0.0.0",4001)){
    e("register board");
    return -4;
  }
  if(0!=SLM_SetLocalIf("192.168.0.1",4001)){
    e("set local interface");
    return -3;
  }
  if(0!=SLM_Connect()){
    e("connect");
    return -2;
  }
  
  if(isnan(status(0)))
    goto disconnect;
  
  if(0!=SLM_LoadConfiguration("/home/martin/cyberpower-mit/mma-essentials-0209/800803_dmdl6_20110215.ini"
			      )){
    e("config");
    goto disconnect;
  }
  if(0!=SLM_LoadCalibrationData("/home/martin/mma-essentials-0209/VC2481_15_67_2011-02-01_0-250nm_Rand7_Typ1.cal")){
    e("calib");
    goto disconnect;
  }
  
  // user ready should start 20us after deflection phase and go low at
  // the same time 
  float d=20.,width=16.;
  if(0!=SLM_SetDeflectionPhase(0.,width*1000.)){
    e("deflection");
    goto disconnect;
  }
  if(0!=SLM_SetExternReady(0.+d,width*1000.-d)){
    e("extern ready");
    goto disconnect;
  }
  if(0!=SLM_EnableExternStart()){
    e("enable extern start");
    goto disconnect;
  }

  if(0!=SLM_SetCycleTime(180.
			 //2.*width+.01
			 )){
    e("cycle time");
    goto disconnect;
  } 

  if(isnan(status(0)))
    goto disconnect;

  if(0!=SLM_SetPowerOn()){
    e("power");
    goto disconnect;
  }
  
  if(0!=SLM_WriteMatrixData(1,3,buf,NN)){ // you have to make sure to upload at least one image
    e("fill");
    goto poweroff;
  }

  if(0!=SLM_SetPictureSequence(1,1,1)){
    e("set-picture-sequence");
    goto poweroff;
  }


  printf("infront of start\n");
  if(0!=SLM_SetStartMMA()){
    e("start");
    goto poweroff;
  }
  // the first time when start-mma is executed, one should wait
  // 100ms..1s (try increasing the delay without matrix until read-status
  // returns the appropriate error)
  usleep(100000);
  // make sure you often call read-status to detect and clear
  // errors. if errors aren't cleared certain functions (like
  // start-mma) will never succeed.

  if(isnan(status(0)))
    goto stop_mma;

  printf("initialized, enter your command:\n");
  char s[CMDLEN],*line;
  // this loop reads text commands and runs them until C-d is pressed
  do{
    line=fgets(s,sizeof(s),stdin);
    printf("retval: %g\n", parse_line(line));
  }while(line);

 stop_mma:
  if(0!=SLM_SetStopMMA())
    e("stop mma");
 poweroff:
  if(0!=SLM_SetPowerOff())
    e("set power off");
 disconnect: 
  if(0!=SLM_Disconnect())
    e("disconnect");
  return 0;
}
