#include "libmma.h"
#include <assert.h>

int
main()
{
  assert(0==connect());
  assert(0==reset());
  assert(0==disconnect());

  return 0;
}
