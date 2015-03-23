#ifndef _MB_STL_MEMORY_
#define _MB_STL_MEMORY_

#if defined(__GNUG__) || defined(_MSC_VER) 

#include <limits.h>

namespace std
{
  template<class T> class numeric_limits
  {
  public:
    inline static T max() throw() {return T();}
  };

  template<> class numeric_limits<long>
  {
  public:
    inline static long max() throw() {return LONG_MAX;}
  };

  template<> class numeric_limits<int>
  {
  public:
    inline static int max() throw() {return INT_MAX;}
    inline static int min() throw() {return INT_MIN;}
  };

  template<> class numeric_limits<double>
  {
  public:
    inline static double max() {return 1.7E100;}
    inline static double min() {return -max();}
  };
} //std
#endif // defined(__GNUG__) || defined(_MSC_VER) 
#endif // _MB_STL_MEMORY_
