
// I copied the function definitions from the manual. 
int SLM_RegisterBoard(unsigned long long,const char*,
		      const char*,const char*,unsigned short);
int SLM_SetLocalIf(const char*,unsigned short);
int SLM_Connect();
int SLM_Disconnect();
int SLM_LoadConfiguration(const char*);
int SLM_SaveConfiguration(const char*);
int SLM_LoadCalibrationData(const char*);
int SLM_SetPowerOn();
int SLM_SetPowerOff();
int SLM_SetStartMMA();
int SLM_SetStopMMA();
int SLM_WriteMatrixData(unsigned short picnum, // 1..1023
			unsigned short buftype, // 3 .. 16bit, 1 .. 24bit bgr
			void*buf, unsigned int length);
int SLM_SetPictureSequence(unsigned short picnum,
			   unsigned short lastpicinseq,
			   unsigned short readyout_p);
int SLM_SetVoltage(unsigned int,float);

int SLM_ReadStatus(unsigned int*,unsigned int*);

int SLM_SetDeflectionPhase(float delay_us, float width_us);
int SLM_SetExternReady(float delay_us,float width_us);
int SLM_EnableExternStart();
int SLM_DisableExternStart();
int SLM_SetCycleTime(float cycletime_ms);
int SLM_Reset();
int SLM_SetParameter(unsigned int paramnumber,void*value,unsigned int bytes);
