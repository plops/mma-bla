#include "libmma.h"
#include <assert.h>

int
main()
{
  assert(0==mma_connect());
  assert(0==mma_reset());
  assert(0==mma_disconnect());

  return 0;
}
