#ifndef __MB_INTERNAL_ITERATOR_H
#define __MB_INTERNAL_ITERATOR_H

#include <iterator>

// Additional function templates that take iterator arguements.
namespace mb
{
  template<class It>
    inline It prec(It i, int count = 1) {while (count-- > 0) --i; return i;}
  
  template<class It>
    inline It succ(It i, int count = 1) {while (count-- > 0) ++i; return i;}
}

// TODO: iterator of iterators

#endif // __MB_INTERNAL_ITERATOR_H
