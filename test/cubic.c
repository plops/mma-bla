#include <malloc.h>
#include <stdio.h>
#include <math.h>
int access_nocheck(int z,int y,int x,int k,int j,int i){
  (void)z;
  return i+x*(j+y*k);
}
inline int clamp(int i,int n){
  if(i<0) return 0;
  if(i>=n) return n-1;
  return i;
}
int access(int z,int y,int x,int k,int j,int i){
  k=clamp(k,z);
  j=clamp(j,y);
  i=clamp(i,x);
  return access_nocheck(z,y,x,k,j,i);  
}
inline void indices_cubic(int f[64],int z,int y,int x,int k,int j,int i){
  int (*fun)()=access_nocheck;
  if(k<=2||j<=2||i<=2||(k>=(z-3))||(j>=(y-3))||(i>=(x-3)))
    fun=access;
  for(int ii=0;ii<4;ii++)
    for(int jj=0;jj<4;jj++)
      for(int kk=0;kk<4;kk++)
	f[access_nocheck(4,4,4,kk,jj,ii)]=fun(z,y,x,k+kk-1,j+jj-1,i+ii-1);
}
inline float dir_y(float*vol,float*wx,int*f,int z,int y){
  return
    wx[0]*vol[f[access_nocheck(4,4,4,z,y,0)]]+
    wx[1]*vol[f[access_nocheck(4,4,4,z,y,1)]]+
    wx[2]*vol[f[access_nocheck(4,4,4,z,y,2)]]+
    wx[3]*vol[f[access_nocheck(4,4,4,z,y,3)]];
}
inline float dir_z(float*vol,float*wy,float*wx,int*f,int z){
  return 
    wy[0]*dir_y(vol,wx,f,z,0)+
    wy[1]*dir_y(vol,wx,f,z,1)+
    wy[2]*dir_y(vol,wx,f,z,2)+
    wy[3]*dir_y(vol,wx,f,z,3);
}
float interpolate_cubic(float*vol,int z,int y,int x,float fk,float fj,float fi){
  float
    k=floor(fk),j=floor(fj),i=floor(fi),
    dz=fk-k,dy=fj-j,dx=fi-i,
    dxx=dx*dx,dxxx=dxx*dx,
    wx[]={ -dx+2*dxx  -dxxx,2   -5*dxx+3*dxxx,  dx+4*dxx-3*dxxx, -dxx  +dxxx },
    dyy=dy*dy,dyyy=dyy*dy,
    wy[]={ -dy+2*dyy  -dyyy,2   -5*dyy+3*dyyy,  dy+4*dyy-3*dyyy, -dyy  +dyyy },
    dzz=dz*dz,dzzz=dzz*dz,
    wz0= -dz+2*dzz  -dzzz,
    wz1=2   -5*dzz+3*dzzz,
    wz2=  dz+4*dzz-3*dzzz,
    wz3=      -dzz  +dzzz;
    auto int f[64];
    indices_cubic(f,z,y,x,(int)k,(int)j,(int)i);
    return 0.125*(wz0*dir_z(vol,wy,wx,f,0)+
		  wz1*dir_z(vol,wy,wx,f,1)+
		  wz2*dir_z(vol,wy,wx,f,2)+
		  wz3*dir_z(vol,wy,wx,f,3));
}
float fun(float x){return 1.1+cos(129.*x);}
int main()
{
  enum{n=128,nh=n/2};
  float*vol;
  vol=malloc(n*n*n*sizeof(*vol));
  for(int i=0;i<n;i++)
    for(int j=0;j<n;j++)
      for(int k=0;k<n;k++){
	float
	  x=(i-nh)*1./n,
	  y=(j-nh)*1./n,
	  z=(k-nh)*1./n,
	  r=sqrt(x*x+y*y+z*z);
	vol[access_nocheck(n,n,n,k,j,i)]=fun(r);
      }
  enum{m=500,mh=m/2};
  for(int ii=0;ii<m;ii++){
    float
      j=64.,
      k=64.,
      y=(j-nh)*1./n,
      z=(k-nh)*1./n,
      x=(ii-mh)*1./m,
      r=sqrt(x*x+y*y+z*z);
      printf("%g %g %g\n",x,
	     interpolate_cubic(vol,n,n,n,k,j,x*n+nh),fun(r));
  }
  free(vol);
  return 0;
}
