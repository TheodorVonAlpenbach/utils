#ifndef _MB_LIST_H
#define _MB_LIST_H

#include <list>
#include "mb_functional_spec.h"
#include <algorithm>

namespace mb {
  
  template<class T>
  inline list<T> range(T a, const T b, const T d = 1) {
    list<T> ret;
    if (a < b)
      std::generate_n(back_inserter(ret), (b-a-1)/d + 1, sequenceGen(a, d));
    return ret;
  }
  
}

#endif //_MB_LIST_H

