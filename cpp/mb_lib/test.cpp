#include "mb_os.h"
#include "mb_list.h"

void main(int nargs, char** args)
{
  size_t a = atoi(args[1]);
  size_t b = atoi(args[2]);
  size_t d = atoi(args[3]);
  mb::print_in(mb::range(a, b, d), cout, " ", "(", ")\n");
}
