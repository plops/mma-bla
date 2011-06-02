// very simplified mma control with quite elaborate error checking
// xpdf /home/martin/ftp.ipms.fraunhofer.de/mma-documentation/linux/UNIX-library_lxIPMS_SLM_v0.93.pdf

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "SLMFunctions.h"

// As opposed to normal convention, functions were successful
// when they return 0.

enum { N=256, // size of MMA array
};


#define e(q) do{fprintf(stderr,"error in file %s:%d in function %s, while calling %s\n",__FILE__,__LINE__,__FUNCTION__,q);}while(0)


int
mma_upload_image(unsigned short*buf)
{
  if(!buf){
    e("buf is null");
    return -1;
  }
  
  // 16bit data
  int pic=1;
  // images can be uploaded without first calling stop-matrix
  if(0!=SLM_WriteMatrixData(pic,3,buf,N*N)){
    e("write-matrix-data");
    return -1;
  }
  free(buf);
  if(0!=SLM_SetPictureSequence(pic,1,1)){
    e("set-picture-sequence");
    return -2;
  }
  return 0;
}

int
mma_fill_constant(unsigned short value)
{
  unsigned short*buf=malloc(N*N*2);
  if(!buf){
    e("malloc");
    return -1;
  }
  int i,j;
  for(j=0;j<N;j++)
    for(i=0;i<N;i++)
      buf[i+N*j]=0;
  for(j=0;j<N;j++)
    for(i=0;i<N;i++){
      float x=(i-N/2)*1./N,y=(j-N/2)*1./N,r2=x*x+y*y;
      if(r2<(.3*.3))
	buf[i+N*j]=value;
    }
  
  return mma_upload_image(buf);
}

int 
mma_set_cycle_time(float time_ms)
{
  
  if(0!=SLM_SetCycleTime(time_ms)){
    e("cycle time");
    return -1;
  }
  return 0;
}

int 
mma_reset()
{
  return SLM_Reset();
}

int
mma_status(unsigned int*stat,unsigned int*error)
{
  if(0!=SLM_ReadStatus(stat,error)){
    e("read-status");
    return -1;
  }
  printf("status %d error %d\n",*stat,*error);
  return 0;
}

int
mma_connect()
{
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
  return 0;
}

int
mma_init()
{
  unsigned int stat,error;
  SLM_InitFunctions();
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
  
  if(0!=mma_status(&stat,&error))
    goto disconnect;
  
  // the ini file must contain the right board id
  if(0!=SLM_LoadConfiguration("/home/martin/cyberpower-mit/mma-essentials-0209/800803_dmdl6_20110215.ini"
			      )){
    e("config");
    goto disconnect;
  }
  
  // If a calibration file is available it should be loaded
  // note there is no way to automatically determine the matrix id
  // if the wrong calibration file is used, a matrix can be run with
  // too high voltages which will destroy the mirrors.
  
  if(0!=SLM_LoadCalibrationData("/home/martin/mma-essentials-0209/VC2481_15_67_2011-02-01_0-250nm_Rand7_Typ1.cal")){
    e("calib");
    goto disconnect;
  }
  // user ready should start 20us after deflection phase and go low at
  // the same time 
  // delay is between trigger in and deflection is 840 us
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
  if(0!=mma_set_cycle_time(2*width+.01))
    goto disconnect;

  if(0!=mma_status(&stat,&error))
    goto disconnect;

  if(0!=SLM_SetPowerOn()){
    e("power");
    goto disconnect;
  }
  // you have to make sure to upload at least one image
  if(0!=mma_fill_constant(4095)){
    e("fill");
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

  if(0!=mma_status(&stat,&error))
    goto stop_mma;
  return 0;
 stop_mma:
  if(0!=SLM_SetStopMMA())
    e("stop mma");
 poweroff:
  if(0!=SLM_SetPowerOff())
    e("set power off");
 disconnect: 
  if(0!=SLM_Disconnect())
    e("disconnect");
  return -1;
}

int
mma_disconnect()
{
  return SLM_Disconnect();
}

int
mma_uninit()
{
  if(0!=SLM_SetStopMMA())
    e("stop mma");
  if(0!=SLM_SetPowerOff())
    e("set power off");
  if(0!=SLM_Disconnect())
    e("disconnect");
  return 0;
}
