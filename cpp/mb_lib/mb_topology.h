#ifndef _TOPOLOGY_H_
#define _TOPOLOGY_H_

#include "point.h"
#include "segment.h"
#include "path.h"
#include "envelope.h"
#include "interval.h"

#ifdef _MSC_VER
#pragma warning(disable : 4786)
#endif // _MSC_VER

namespace mb
{

  template<class T>
    inline bool touch(const interval<T>& lhs, const interval<T>& rhs)
    {return lhs.max() == rhs.min() || rhs.max() == lhs.min();}

  template<class P>
    inline bool touch(const segment<P>& lhs, const segment<P>& rhs)
    {
      const P& lf = lhs.front(); const P& lb = lhs.back();
      const P& rf = rhs.front(); const P& rb = rhs.back();
      return
	(lf == rf && disjoint(lb, rhs) && disjoint(rb, lhs)) ||
	(lf == rb && disjoint(lb, rhs) && disjoint(rf, lhs)) ||
	(lb == rf && disjoint(lf, rhs) && disjoint(rb, lhs)) ||
	(lb == rb && disjoint(lf, rhs) && disjoint(rf, lhs));
    }

  template<class T>
    inline bool disjoint(const interval<T>& lhs, const interval<T>& rhs)
    {return lhs.max() < rhs.min() || rhs.max() < lhs.min();}

  template<class T>
    inline bool disjoint(const envelope<T>& lhs, const envelope<T>& rhs)
    {return disjoint(lhs.xI(), rhs.xI()) || disjoint(lhs.yI(), rhs.yI());}

  template<class T>
    inline bool overlap(const interval<T>& lhs, const interval<T>& rhs)
    {return !disjoint(lhs, rhs) && !touch(lhs, rhs);}

  template<class T>
    inline bool overlap(const envelope<T>& lhs, const envelope<T>& rhs)
    {return overlap(lhs.xI(), rhs.xI()) && overlap(lhs.yI(), rhs.yI());}

  template<class T>
    inline bool overlap(const T x, const interval<T>& i)
    {mb::Error("mb::overlap(T,i<T>))", "Call makes no sense!");
    return !i.contains(x);} //no
  
  template<class T>
    inline bool overlap(const point<T>& p, const envelope<T>& e)
    {return !(e.containsX(p.x()) && e.containsY(p.y()));}
  
  template<class T, class P>
    inline bool overlap(const point<T>& p, const segment<P>& s)
    {return (overlap(p, envelopeOf(s))) ||
       (dx(s)*(p.y()-s.front().y() != dy(s)*(p.x()-s.front().x())));}
  
  template<class P>
    inline bool overlap(const segment<P>& lhs, const segment<P>& rhs)
    {return
       (overlap(lhs.front(), rhs) && disjoint(lhs.back(), rhs)) || 
       (overlap(lhs.back(), rhs) && disjoint(lhs.front(), rhs));}

  template<class T>
    inline interval<T> g_union(const interval<T>& lhs, const interval<T>& rhs)
    {return interval<T>(min(lhs.min(), rhs.min()), max(lhs.max(), rhs.max()));}

  template<class T>
    inline interval<T> g_union(const interval<T>& lhs, const T x)
    {return interval<T>(min(lhs.min(), x), max(lhs.max(), x));}

  template<class T>
    inline envelope<T> g_union(const envelope<T>& lhs, const envelope<T>& rhs)
    {
      return envelope<T>(g_union(lhs.xI(), rhs.xI()),
			 g_union(lhs.yI(), rhs.yI()));
    }

  template<class T>
    inline envelope<T> g_union(const envelope<T>& lhs, const point<T>& p)
    {
      return envelope<T>(g_union(lhs.xI(), p.x()),
			 g_union(lhs.yI(), p.y()));
    }

} //mb

#endif //ifndef _TOPOLOGY_H_

