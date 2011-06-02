int mma_upload_image(unsigned short*buf);
int mma_set_cycle_time(float time_ms);
int mma_init();
int mma_uninit();
int mma_status(unsigned int*status,unsigned int*error);
int mma_reset();
int mma_connect();
int mma_disconnect();
