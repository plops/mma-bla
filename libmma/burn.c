#include "libmma.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "slm.h"

enum{N=256,NN=N*N};

// echo "echo 4 > /proc/sys/net/ipv4/tcp_fin_timeout"  | sudo sh

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

  int i,j;
  unsigned short*buf=(unsigned short*)malloc(NN*sizeof(*buf));
  assert(buf);
  splat(5,0,12,buf);
  
  assert(0==mma_init());

  for(i=0;i<256;i+=8)
    for(j=0;j<256;j+=8){
      printf("%d %d\n",i,j);
      splat(i,j,7,buf);
      if(0!=mma_upload_image(buf)){
	printf("error upload-image\n");
      }
    }
  assert(0==mma_uninit());
  return 0;
}
