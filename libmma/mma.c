#include "libmma.h"
#include <assert.h>

int
main()
{
  assert(0==init());
  sleep(3);
  assert(0==uninit());
  return 0;
}
