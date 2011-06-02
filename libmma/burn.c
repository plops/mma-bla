#include "libmma.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "slm.h"

enum{N=256,NN=N*N};


unsigned short*
splat(int i,int j, int d,unsigned short*buf)
{
  int x,y;
  for(x=0;x<NN;x++)
    buf[i]=90;
  for(y=-d;y<=d;y++)
    for(x=-d;x<=d;x++){
      int xx=x+i,yy=y+j;
      if((0<=xx) && (xx<N) && (0<=yy) && (y<N))
	buf[i+N*j]=4095;
    }
  return buf;
}

int
main()
{

  int i,j;
  assert(0==init());
  printf("initialized\n");
  usleep(3000000);
  unsigned short*buf=malloc(N*N*2);
  printf("before\n");
  //SLM_SetStopMMA();
  SLM_WriteMatrixData(1,3,buf,NN);
  //if(0!=SLM_WriteMatrixData(1,3,buf,N*N)){
  //printf("error upload-image\n");
  //}
  //SLM_SetStartMMA();
  printf("after\n");
  if(0) for(i=0;i<6;i+=4)
    for(j=0;j<6;j+=4){
      printf("%d %d\n",i,j);
      splat(i,j,7,buf);
      if(0!=SLM_WriteMatrixData(1,3,buf,N*N)){
	printf("error upload-image\n");
      }
      usleep(100000);
    }
  assert(0==uninit());
  return 0;
}
