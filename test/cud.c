#include <complex.h>
#include <math.h>
#include <cufft.h>
#include <cuda_runtime_api.h>
#include <fcntl.h>
#include <stdio.h>

enum {nx=80,ny=nx,nz=nx,n3=nx*ny*nz};

inline row_major_index2(int y,int x,int ny,int nx)
{
  return y+ny*x;
}

inline row_major_index3(int z,int y,int x,int nz,int ny,int nx)
{
  return z+nz*(y+ny*x);
}

#define DORECTANGLE(i,j,ax,bx,ay,by) for(i=ax;i<bx;i++)for(j=ay;j<by;j++)
#define DOBOX(i,j,k,ax,bx,ay,by,az,bz) for(i=ax;i<bx;i++)for(j=ay;j<by;j++)for(k=az;k<bz;k++)

// http://qjmath.oxfordjournals.org/cgi/reprint/12/1/165.pdf
// ft of sphere
void
fill(complex float*d,int nz,int ny,int nx,float radius)
{
  int i,j,k;
  DOBOX(i,j,k,0,nx,0,ny,0,nz){
    float
      x=(i-nx/2.)*1./nx,
      y=(j-ny/2.)*1./ny,
      z=(k-nz/2.)*1./nz,
      r=sqrtf(x*x+y*y+z*z),
      arg=2.*M_PI*r*radius;
    if(arg!=0.0)
      d[row_major_index3(k,j,i,nz,ny,nx)]=3.*(sinf(arg)-cosf(arg))/(arg*arg);
    else
      d[row_major_index3(k,j,i,nz,ny,nx)]=0.;
  }
}

void
fftshift2(complex float*out,complex float*in,int ny,int nx)
{
  int
    hx=nx/2,hy=ny/2,
    i,j;
  DORECTANGLE(j,i,0,ny,0,nx){
    int 
      ii=(i+hx)%nx,
      jj=(j+hy)%ny;
    out[row_major_index2(j,i,ny,nx)]=
      in[row_major_index2(jj,ii,ny,nx)];
  }
}

void
fftshift3(complex float*out,complex float*in,int nz,int ny,int nx)
{
  int
    hx=nx/2,hy=ny/2,hz=nz/2,
    i,j,k;
  DOBOX(k,j,i,0,nz,0,ny,0,nx){
    int 
      ii=(i+hx)%nx,
      jj=(j+hy)%ny,
      kk=(k+hz)%nz;
    out[row_major_index3(k,j,i,nz,ny,nx)]=
      in[row_major_index3(kk,jj,ii,nz,ny,nx)];
  }
}

float
max_realpart(complex float*d,int n)
{
  float m=creal(d[0]);
  int i;
  for(i=1;i<n;i++){
    double v=creal(d[i]);
    m=v>m?v:m;
  }
  return m;
}

float
min_realpart(complex float*d,int n)
{
  float m=creal(d[0]);
  int i;
  for(i=1;i<n;i++){
    double v=creal(d[i]);
    m=v<m?v:m;
  }
  return m;
}

void
write_pgm_ub8(char*fn,unsigned char*a,int ny,int nx)
{
  FILE*f=fopen(fn,"w");
  fprintf(f,"P5 %d %d 255\n",nx,ny);
  fwrite(a,nx,ny,f);
  fclose(f);
}

void
save_stack(char*fn,complex float*a,int nz,int ny,int nx,float(*fun)(complex float))
{
  if(!fun)
    fun=crealf;
  unsigned char buf[nx*ny];
  float
    ma=max_realpart(a,nx*ny*nz),
    mi=min_realpart(a,nx*ny*nz);
  int i,j,k;
  for(k=0;k<nz;k++){
    DORECTANGLE(i,j,0,nx,0,ny)
      buf[row_major_index2(j,i,ny,nx)]=
      (unsigned char)(255.*(fun(a[row_major_index3(k,j,i,nz,ny,nx)])-mi)
		      /(ma-mi));
    char s[100];
    snprintf(s,sizeof(s),"%s_%03d.pgm",fn,k);
    write_pgm_ub8(s,buf,ny,nx);
  }
}

void
fft(complex float*data,int nz,int ny,int nx)
{
  cufftHandle plan;
  cufftComplex *idata,*odata;
  int n=sizeof(complex float)*nx*ny*nz;
  cudaMalloc((void**) &idata,n);
  cudaMemcpy(idata,data,n,cudaMemcpyHostToDevice);
  
  printf("(idata %d)\n",idata);

  cudaMalloc((void**) &odata,n);

  printf("(odata %d)\n",odata);

  cufftPlan3d(&plan,nx,ny,nz,CUFFT_C2C);
  
  printf("(plan %d)\n",plan);

  cufftExecC2C(plan,idata,odata,CUFFT_FORWARD);

  cudaMemcpy((void*) data,odata,n,
	     cudaMemcpyDeviceToHost);

  cufftDestroy(plan);
  cudaFree(idata);
  cudaFree(odata);
}

unsigned long long int rdtsc(void)
{
   unsigned long long int x;
   unsigned a, d;

   __asm__ volatile("rdtsc" : "=a" (a), "=d" (d));

   return ((unsigned long long)a) | (((unsigned long long)d) << 32);;
}


int
main()
{
  complex float data[n3],data2[n3];

  fill(data,nz,ny,nx,25.);

  //save_stack("input",data,nz,ny,nx,0);

  fftshift3(data2,data,nz,ny,nx);

  //save_stack("input-shift",data2,nz,ny,nx,0);

  printf("%g %g\n",min_realpart(data2,n3),max_realpart(data2,n3));
  
  unsigned long long start=rdtsc();
  sleep(1);
  double ticks_per_sec=1.*(rdtsc()-start);
 

  start=rdtsc();
  fft(data2,nz,ny,nx);
  double ticks=1.*(rdtsc()-start);
  printf("rdtsc %g ticks = %g s\n",ticks,ticks/ticks_per_sec);

  fftshift3(data,data2,nz,ny,nx);
  
  printf("%g %g\n",min_realpart(data,n3),max_realpart(data,n3));
  save_stack("vol",data,nz,ny,nx,0);
  return 0;
}
