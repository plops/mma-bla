#include "libmma.h"
#include <assert.h>

int
main()
{
  assert(0==mma_init());
  sleep(3);
  assert(0==mma_uninit());
  return 0;
}
