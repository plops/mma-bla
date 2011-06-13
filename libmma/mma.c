#include "libmma.h"
#include <assert.h>

int
main()
{
  assert(0==mma_init());
  assert(0==mma_uninit());
  return 0;
}
