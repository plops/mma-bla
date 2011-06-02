#ifndef SLMFUNCTIONS_H_
#define SLMFUNCTIONS_H_

/*
 * SLMFunctions.h
 *
 * functions declaration for dlopen,dlsym
 * in use with libIPMS_SLM.so.x.x.x
 *
 *  Created on: 01.09.2009
 *      Author: Mark Eckert
 *
 *
 *  (C) Fraunhofer IPMS Dresden, Germany
 *
 * v0.2 added SLM_WriteMatrixData
 */

#include <stdint.h>

//indices
#define SLM_SMART_IDX_VFRAME_F	0
#define SLM_SMART_IDX_VFRAME_L	1
#define SLM_SMART_IDX_VCAP_F	2
#define SLM_SMART_IDX_VCAP_L	3
#define SLM_SMART_IDX_VDMD_F	4
#define SLM_SMART_IDX_VDMD_L	5
#define SLM_SMART_IDX_VSHIELD_F	6
#define SLM_SMART_IDX_VSHIELD_L	7
#define SLM_SMART_IDX_VCE_F		8
#define SLM_SMART_IDX_VCE_L		9

#define SLM_PIXEL_VOLTAGE_IDX	30
#define SLM_VCALL_VOLTAGE_IDX	31

//dlopen specs
typedef int (*SLM_REGISTERBOARD		) (uint64_t,const char*,const char*, const char*, unsigned short);
typedef int (*SLM_SETLOCALIF   		) (const char*,unsigned short);
typedef int (*SLM_CONNECT			) ();
typedef int (*SLM_DISCONNECT		) ();
typedef int (*SLM_SETVOLTAGE		) (unsigned short,  float );
typedef int (*SLM_GETVOLTAGE		) (unsigned short,  float*);
typedef int (*SLM_SETPOWERON		) ();
typedef int (*SLM_SETPOWEROFF		) ();
typedef int (*SLM_SETSTARTMMA		) ();
typedef int (*SLM_SETSTOPMMA		) ();
typedef int (*SLM_ENABLEEXTERNSTART	) ();
typedef int (*SLM_DISABLEEXTERNSTART) ();
typedef int (*SLM_SETDEFLECTIONPHASE) ( float  delay_us, float  width_us );
typedef int (*SLM_GETDEFLECTIONPHASE) ( float *delay_us, float *width_us );
typedef int (*SLM_SETEXTERNREADY	) ( float  delay_us, float  width_us );
typedef int (*SLM_GETEXTERNREADY	) ( float *delay_us, float *width_us );
typedef int (*SLM_LOADCONFIGURATION ) ( const char* );
typedef int (*SLM_SAVECONFIGURATION ) ( const char* );
typedef int (*SLM_LOADPICTURE		) ( const char *,  unsigned short );
typedef int (*SLM_WRITEMATRIXDATA	) ( unsigned short, unsigned short, void*, unsigned int );
typedef int (*SLM_READSTATUS		) ( unsigned int*, unsigned int*  );
typedef int (*SLM_SERVICECOMMAND	) ( char *command, void *data, unsigned int data_len );
typedef int (*SLM_ENABLEMICROSCOPEMODE  ) ();
typedef int (*SLM_DISABLEMICROSCOPEMODE ) ();
typedef int (*SLM_SETPICTURESEQUENCE	) ( unsigned short, unsigned short, unsigned short  );
typedef int (*SLM_SETMATRIXCLOCK	) ( float  FreqMHz, float  ClockDataDelay_ns, float  PeriodWidth_ns);
typedef int (*SLM_GETMATRIXCLOCK	) ( float *FreqMHz, float *ClockDataDelay_ns, float *PeriodWidth_ns);
typedef int (*SLM_SETMMATEMPERATURE	) ( float  TGrad );
typedef int (*SLM_GETMMATEMPERATURE	) ( float *TGrad );
typedef int (*SLM_SWITCHPELTIERON	) ();
typedef int (*SLM_SWITCHPELTIEROFF	) ();
typedef int (*SLM_UPDATEFPGADESIGN	) ( const char* );
typedef int (*SLM_UPDATEEMBEDDEDSW	) ( const char* );
typedef int (*SLM_LOADCALIBRATIONDATA	) ( const char* );
typedef int (*SLM_SETPARAMETER		) ( unsigned int ParamNumber, void* ParamValue, unsigned int ParamNumberr );
typedef int (*SLM_GETPARAMETER		) ( unsigned int ParamNumber, void* ParamValue, unsigned int ParamNumberr );
typedef int (*SLM_SETCYCLETIME          ) ( float time_ms );
typedef int (*SLM_RESET                 ) ();

extern SLM_REGISTERBOARD 		SLM_RegisterBoard;
extern SLM_SETLOCALIF	  		SLM_SetLocalIf;
extern SLM_CONNECT		  		SLM_Connect;
extern SLM_DISCONNECT			SLM_Disconnect;
extern SLM_SETVOLTAGE	  		SLM_SetVoltage;
extern SLM_GETVOLTAGE			SLM_GetVoltage;
extern SLM_SETPOWERON	  		SLM_SetPowerOn;
extern SLM_SETPOWEROFF			SLM_SetPowerOff;
extern SLM_SETSTARTMMA			SLM_SetStartMMA;
extern SLM_SETSTOPMMA			SLM_SetStopMMA;
extern SLM_ENABLEEXTERNSTART	SLM_EnableExternStart;
extern SLM_DISABLEEXTERNSTART	SLM_DisableExternStart;
extern SLM_SETDEFLECTIONPHASE	SLM_SetDeflectionPhase;
extern SLM_GETDEFLECTIONPHASE	SLM_GetDeflectionPhase;
extern SLM_SETEXTERNREADY		SLM_SetExternReady;
extern SLM_GETEXTERNREADY		SLM_GetExternReady;
extern SLM_LOADCONFIGURATION	SLM_LoadConfiguration;
extern SLM_SAVECONFIGURATION	SLM_SaveConfiguration;
extern SLM_LOADPICTURE			SLM_LoadPicture;
extern SLM_WRITEMATRIXDATA		SLM_WriteMatrixData;
extern SLM_READSTATUS			SLM_ReadStatus;
extern SLM_SERVICECOMMAND		SLM_ServiceCommand;
extern SLM_ENABLEMICROSCOPEMODE 	SLM_EnableMicroscopeMode;
extern SLM_DISABLEMICROSCOPEMODE	SLM_DisableMicroscopeMode;
extern SLM_SETPICTURESEQUENCE		SLM_SetPictureSequence;
extern SLM_SETMATRIXCLOCK		SLM_SetMatrixClock;
extern SLM_GETMATRIXCLOCK		SLM_GetMatrixClock;
extern SLM_SETMMATEMPERATURE		SLM_SetMMATemperature;
extern SLM_GETMMATEMPERATURE		SLM_GetMMATemperature;
extern SLM_SWITCHPELTIERON		SLM_SwitchPeltierOn;
extern SLM_SWITCHPELTIEROFF		SLM_SwitchPeltierOff;
extern SLM_UPDATEFPGADESIGN		SLM_UpdateFPGADesign;
extern SLM_UPDATEEMBEDDEDSW		SLM_UpdateEmbeddedSW;
extern SLM_LOADCALIBRATIONDATA		SLM_LoadCalibrationData;
extern SLM_SETPARAMETER			SLM_SetParameter;
extern SLM_GETPARAMETER			SLM_GetParameter;
extern SLM_SETCYCLETIME                 SLM_SetCycleTime;
extern SLM_RESET                        SLM_Reset;
//loader
extern int SLM_InitFunctions ();
//extern uint64_t char2uint64		  (const char*);

#endif /* SLMFUNCTIONS_H_ */
