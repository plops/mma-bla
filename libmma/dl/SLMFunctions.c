/*
 * SLMFunctions.c
 *
 *  Created on: 01.09.2009
 *      Author: Mark Eckert
 *
 * (C) Fraunhofer IPMS Dresden, Germany
 *
 * v0.2 added SLM_WriteMatrixData
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <dlfcn.h>

char libIPMS_SLMVersion[] = "/home/martin/mma-essentials-0209/libIPMS_SLM.so.1.0.0";

#include "SLMFunctions.h"

SLM_REGISTERBOARD 		SLM_RegisterBoard		=0;
SLM_SETLOCALIF	  		SLM_SetLocalIf			=0;
SLM_CONNECT		  		SLM_Connect				=0;
SLM_DISCONNECT			SLM_Disconnect			=0;
SLM_SETVOLTAGE	  		SLM_SetVoltage			=0;
SLM_GETVOLTAGE	  		SLM_GetVoltage			=0;
SLM_SETPOWERON	  		SLM_SetPowerOn			=0;
SLM_SETPOWEROFF			SLM_SetPowerOff 		=0;
SLM_SETSTARTMMA			SLM_SetStartMMA			=0;
SLM_SETSTOPMMA			SLM_SetStopMMA			=0;
SLM_ENABLEEXTERNSTART	SLM_EnableExternStart	=0;
SLM_DISABLEEXTERNSTART	SLM_DisableExternStart	=0;
SLM_SETDEFLECTIONPHASE	SLM_SetDeflectionPhase	=0;
SLM_GETDEFLECTIONPHASE	SLM_GetDeflectionPhase	=0;
SLM_SETEXTERNREADY		SLM_SetExternReady		=0;
SLM_GETEXTERNREADY		SLM_GetExternReady		=0;
SLM_LOADCONFIGURATION	SLM_LoadConfiguration	=0;
SLM_SAVECONFIGURATION	SLM_SaveConfiguration	=0;
SLM_LOADPICTURE			SLM_LoadPicture			=0;
SLM_WRITEMATRIXDATA		SLM_WriteMatrixData =0;
SLM_READSTATUS			SLM_ReadStatus			=0;
SLM_SERVICECOMMAND	SLM_ServiceCommand=0;
SLM_ENABLEMICROSCOPEMODE	SLM_EnableMicroscopeMode=0;
SLM_DISABLEMICROSCOPEMODE	SLM_DisableMicroscopeMode=0;
SLM_SETPICTURESEQUENCE		SLM_SetPictureSequence=0;
SLM_SETMATRIXCLOCK		SLM_SetMatrixClock=0;
SLM_GETMATRIXCLOCK		SLM_GetMatrixClock=0;
SLM_SETMMATEMPERATURE		SLM_SetMMATemperature=0;
SLM_GETMMATEMPERATURE		SLM_GetMMATemperature=0;
SLM_SWITCHPELTIERON		SLM_SwitchPeltierOn=0;
SLM_SWITCHPELTIEROFF		SLM_SwitchPeltierOff=0;
SLM_UPDATEFPGADESIGN		SLM_UpdateFPGADesign=0;
SLM_UPDATEEMBEDDEDSW		SLM_UpdateEmbeddedSW=0;
SLM_LOADCALIBRATIONDATA		SLM_LoadCalibrationData=0;
SLM_SETPARAMETER		SLM_SetParameter=0;
SLM_GETPARAMETER		SLM_GetParameter=0;
SLM_SETCYCLETIME                SLM_SetCycleTime=0;
SLM_RESET                       SLM_Reset=0;

/* uint64_t char2uint64 (const char* in) */
/* { */
/* 	if ( in == NULL ) return -1; */

/* 	int 		i 	 =0; */
/* 	int		    shift=60; */
/* 	uint64_t 	id	 =0; */

/* 	for ( i=0; i<strlen(in); i++ ) */
/* 	{ */
/* 		if ( '0' <= in[i] && in[i] <= '9') */
/* 		{ */
/* 			id += ((uint64_t)(in[i]-'0')) << shift; */
/* 		} */
/* 		else */
/* 		if ( 'A' <= in[i] && in[i] <= 'F' ) */
/* 		{ */
/* 			id += ((uint64_t)((in[i]-'A')+10)) << shift; */
/* 		} */
/* 		else */
/* 			return -1; */

/* 		shift -= 4; */
/* 	} */

/* 	return id; */
/* } */

//function loader
int SLM_InitFunctions ()
{
	void *handle;
	void *error;

	int ierror=0;

	handle = dlopen (libIPMS_SLMVersion, RTLD_LAZY);
	if (!handle )
	{
		return -1;
	}
	else
	{
		printf ("lib IPMS_SLM loaded.\r\n");
	}

	///////////////////////////////////
	//load functions
	///////////////////////////////////

	dlerror(); //clear errors
	SLM_RegisterBoard = (SLM_REGISTERBOARD) dlsym (handle,"SLM_RegisterBoard");

	if ( (error = dlerror()) != NULL )
	{
		printf ("error loading SLM_RegisterBoard %s\r\n",(char*) error );
		ierror +=-1;
	}

	SLM_SetLocalIf = (SLM_SETLOCALIF) dlsym (handle,"SLM_SetLocalIf");

	if ( (error = dlerror()) != NULL )
	{
		printf ("error loading SLM_SetLocalIf %s\r\n",(char*) error );
		ierror+=-1;
	}

	SLM_Connect = (SLM_CONNECT) dlsym (handle,"SLM_Connect");

	if ( (error = dlerror()) != NULL )
	{
		printf ("error loading SLM_SearchBoard %s\r\n",(char*) error);
		ierror+=-1;
	}

	SLM_Disconnect = (SLM_DISCONNECT) dlsym(handle,"SLM_Disconnect");

	if  ((error = dlerror()) != NULL )
	{
		printf ("error loading SLM_Disconnect %s\r\n",(char*) error);
		ierror+=-1;
	}

	SLM_SetVoltage = (SLM_SETVOLTAGE) dlsym (handle,"SLM_SetVoltage");

	if ( (error = dlerror()) != NULL )
	{
		printf ("error loading SLM_SetVoltage %s\r\n",(char*) error);
		ierror+=-1;
	}

	SLM_GetVoltage = (SLM_GETVOLTAGE) dlsym (handle,"SLM_GetVoltage");

	if ( (error = dlerror()) != NULL )
	{
		printf ("error loading SLM_GetVoltage %s\r\n",(char*) error);
		ierror+=-1;
	}

	SLM_SetPowerOn = (SLM_SETPOWERON) dlsym (handle,"SLM_SetPowerOn");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_SetPowerOn %s\r\n", (char*) error);
		ierror+=-1;
	}

	SLM_SetPowerOff = (SLM_SETPOWEROFF) dlsym (handle,"SLM_SetPowerOff");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_SetPowerOff %s\r\n", (char*) error);
		ierror+=-1;
	}

	SLM_SetStartMMA = (SLM_SETSTARTMMA) dlsym (handle,"SLM_SetStartMMA");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_SetStartMMA %s\r\n", (char*) error);
		ierror+=-1;
	}

	SLM_SetStopMMA = (SLM_SETSTOPMMA) dlsym (handle,"SLM_SetStopMMA");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_SetStopMMA %s\r\n", (char*) error);
		ierror+=-1;
	}

	SLM_EnableExternStart = (SLM_ENABLEEXTERNSTART) dlsym (handle,"SLM_EnableExternStart");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_EnableExternStart %s\r\n", (char*) error);
		ierror+=-1;
	}

	SLM_DisableExternStart = (SLM_DISABLEEXTERNSTART) dlsym (handle,"SLM_DisableExternStart");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_DisableExternStart %s\r\n", (char*) error);
		ierror+=-1;
	}

	SLM_SetStartMMA = (SLM_SETSTARTMMA) dlsym (handle,"SLM_SetStartMMA");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_SetStartMMA %s\r\n", (char*) error);
		ierror+=-1;
	}

	SLM_SetDeflectionPhase = (SLM_SETDEFLECTIONPHASE) dlsym (handle,"SLM_SetDeflectionPhase");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_SetDeflectionPhase %s\r\n", (char*) error);
		ierror+=-1;
	}

	SLM_GetDeflectionPhase = (SLM_GETDEFLECTIONPHASE) dlsym(handle,"SLM_GetDeflectionPhase");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_GetDeflectionPhase %s\r\n", (char*) error);
		ierror+=-1;
	}

	SLM_SetExternReady = (SLM_SETEXTERNREADY) dlsym (handle,"SLM_SetExternReady");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_SetExternReady %s\r\n", (char*) error);
		ierror+=-1;
	}

	SLM_GetExternReady = (SLM_GETEXTERNREADY) dlsym (handle,"SLM_GetExternReady");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_GetExternReady %s\r\n", (char*) error);
		ierror+=-1;
	}

	SLM_LoadConfiguration = (SLM_LOADCONFIGURATION) dlsym (handle, "SLM_LoadConfiguration");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_LoadConfiguratoin %s\r\n",(char*) error);
		ierror+=-1;
	}

	SLM_SaveConfiguration = (SLM_SAVECONFIGURATION) dlsym (handle, "SLM_SaveConfiguration");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_SaveConfiguratoin %s\r\n",(char*) error);
		ierror+=-1;
	}

	SLM_LoadPicture	= ( SLM_LOADPICTURE ) dlsym (handle, "SLM_LoadPicture");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_LoadPicture %s\r\n", (char*)error);
		ierror+=-1;
	}

	SLM_ReadStatus = ( SLM_READSTATUS ) dlsym (handle, "SLM_ReadStatus" );
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_ReadStatus %s\r\n",(char*)error);
		ierror+=-1;
	}

	SLM_ServiceCommand = (SLM_SERVICECOMMAND) dlsym (handle,"SLM_ServiceCommand");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_ServiceCommand %s\r\n",(char*)error);
		ierror+=-1;
	}

	SLM_EnableMicroscopeMode = (SLM_ENABLEMICROSCOPEMODE) dlsym (handle,"SLM_EnableMicroscopeMode");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_EnableMicroscopeMode %s\r\n", (char*)error);
		ierror+=-1;
	}
	
	SLM_DisableMicroscopeMode = (SLM_DISABLEMICROSCOPEMODE) dlsym (handle,"SLM_DisableMicroscopeMode");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_DisableMicroscopeMode %s\r\n", (char*)error);
		ierror+=-1;
	}
	
	SLM_SetPictureSequence = (SLM_SETPICTURESEQUENCE) dlsym (handle,"SLM_SetPictureSequence");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_SetPictureSequence %s\r\n", (char*)error);
		ierror+=-1;
	}

	SLM_SetMatrixClock	= (SLM_SETMATRIXCLOCK) dlsym (handle,"SLM_SetMatrixClock");
	if ( (error=dlerror()) != NULL )
	{	
		printf ("error loading SLM_SetMatrixClock %s\r\n", (char*)error);
		ierror+=-1;
	}	
	
	SLM_GetMatrixClock	= (SLM_GETMATRIXCLOCK) dlsym (handle,"SLM_GetMatrixClock");
	if ( (error=dlerror()) != NULL )
	{	
		printf ("error loading SLM_GetMatrixClock %s\r\n", (char*)error);
		ierror+=-1;
	}
	SLM_SetMMATemperature  = (SLM_SETMMATEMPERATURE) dlsym (handle,"SLM_SetMMATemperature");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_SetMMATempereture %s\r\n",(char*) error );
		ierror+=-1;
	}
	SLM_GetMMATemperature = (SLM_GETMMATEMPERATURE) dlsym (handle,"SLM_GetMMATemperature");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_GetMMATemperature %s\r\n",(char*) error );
		ierror += -1;
	}

	SLM_SwitchPeltierOn	= (SLM_SWITCHPELTIERON) dlsym (handle,"SLM_SwitchPeltierOn");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_SwitchPeltierOn %s\r\n",(char*) error );
		ierror+=-1;
	}
	SLM_SwitchPeltierOff	= (SLM_SWITCHPELTIEROFF) dlsym (handle,"SLM_SwitchPeltierOff");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_SwitchPeltierOff %s\r\n",(char*) error );
		ierror+=-1;
	}
	SLM_WriteMatrixData = (SLM_WRITEMATRIXDATA) dlsym ( handle,"SLM_WriteMatrixData");
	if ( (error=dlerror()) != NULL )
	{	
		printf ("error loading SLM_WriteMatrixData %s\r\n", (char*) error );
		ierror+=-1;
	}
	SLM_UpdateFPGADesign = (SLM_UPDATEFPGADESIGN) dlsym ( handle, "SLM_UpdateFPGADesign" );
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_UpdateFPGADesign %s\r\n", (char*) error );
		ierror += 1;
	}
	SLM_UpdateEmbeddedSW = (SLM_UPDATEEMBEDDEDSW) dlsym ( handle,"SLM_UpdateEmbeddedSW");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_UpdateEmbeddedSW %s\r\n", (char*) error );
		ierror+=1;
	}
	SLM_LoadCalibrationData = (SLM_LOADCALIBRATIONDATA) dlsym ( handle, "SLM_LoadCalibrationData");
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_LoadCalibrationData %s\r\n", (char*) error );
		ierror+=1;
	}
	SLM_SetParameter = (SLM_SETPARAMETER) dlsym ( handle, "SLM_SetParameter" );
	if ( (error=dlerror()) != NULL )	
	{
		printf ("error loading SLM_SetParameter %s\r\n", (char*) error );
		ierror++;
	}
	SLM_GetParameter = (SLM_GETPARAMETER) dlsym ( handle, "SLM_GetParameter" );
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_GetParameter %s\r\n", (char*) error );
		ierror++;
	}
	SLM_SetCycleTime = (SLM_SETCYCLETIME) dlsym (handle, "SLM_SetCycleTime" );
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_SetCycleTime %s\r\n", (char*) error );
		ierror++;
	}
	SLM_Reset = (SLM_RESET) dlsym (handle, "SLM_Reset" );
	if ( (error=dlerror()) != NULL )
	{
		printf ("error loading SLM_Reset %s\r\n", (char*) error );
		ierror++;
	}
	return ierror;
}



