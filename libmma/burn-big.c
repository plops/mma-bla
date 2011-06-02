#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "slm.h"

enum{N=256,NN=N*N};

#define e(q) do{fprintf(stderr,"error in file %s:%d in function %s, while calling %s\n",__FILE__,__LINE__,__FUNCTION__,q);}while(0)


int
status()
{
  unsigned int stat,error;
  if(0!=SLM_ReadStatus(&stat,&error)){
    e("read-status");
    return -1;
  }
  printf("status %d error %d\n",stat,error);
  return 0;
}


unsigned short*
splat(int i,int j, int d,unsigned short*buf)
{
  int x,y;
  for(x=0;x<NN;x++)
    buf[x]=90;
  for(y=-d;y<=d;y++)
    for(x=-d;x<=d;x++){
      int xx=x+i,yy=y+j;
      if((0<=xx) && (xx<N) && (0<=yy) && (yy<N))
	buf[xx+N*yy]=4095;
    }
  return buf;
}

int
main()
{
  unsigned short*buf=malloc(N*N*2);
  splat(0,0,10,buf);
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
  
  if(0!=status())
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

  if(0!=status())
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

  if(0!=status())
    goto stop_mma;

  int i,j;
  for(i=0;i<N;i+=1)
    for(j=0;j<N;j+=1){
      printf("%d %d\n",i,j);
      splat(i,j,3,buf);
      if(0!=SLM_WriteMatrixData(1,3,buf,N*N)){
	printf("error upload-image\n");
      }
    }
 
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
