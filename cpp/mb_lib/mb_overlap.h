#ifndef _OVERLAP_H_
#define _OVERLAP_H_

#include "point.h"
#include "segment.h"
#include "path.h"
#include "envelope.h"
#include "interval.h"

namespace mb {

  template<class T>
  inline bool overlap(const T x, const interval<T>& i) { 
    return !i.contains(x); }
  
  template<class T>
  inline bool overlap(const point<T>& p, const envelope<T>& e) {
    return !(e.containsX(p.x()) && e.containsY(p.y())); }
  
  template<class T, class P>
  inline bool overlap(const point<T>& p, const segment<P>& s) {
    return (overlap(p, envelopeOf(s))) ||
      (dx(s)*(p.y()-s.front().y() != dy(s)*(p.x()-s.front().x())));}

} //mb

#endif //ifndef _OVERLAP_H_
