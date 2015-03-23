#ifndef _PATH_H_
#define _PATH_H_

#include "iterator_segment.h"
#include <list>
#include "point.h"

namespace mb
{
  template<class P, class C = std::list<P> >
    class path : public C
    {
    public:
      typedef P point_type;
      typedef typename C::const_iterator CPI;
      typedef const_segment_iterator<CPI> const_s_iterator;

      path() : C() {}
      path(unsigned int size) : C(size) {}
      path(const_s_iterator b, const_s_iterator e) : C()
      {
	if (b == e) return;
	push_back(b->front());
	while (b != e)
	{
	  push_back(b->back());
	  ++b;
	}
      }
      // constructor: check that path is ok!
      const_s_iterator beginS() const {return begin();}
      const_s_iterator endS() const {return --end();}
      typename const_s_iterator::value_type frontS() const {return *beginS();}
      typename const_s_iterator::value_type backS() const {return *(--endS());}

      void push_path_front(const path& pa)
      {copy(pa.rbegin(), pa.rend(), front_inserter(*this));}

      // iterator solution not good. frontS const  and backS const should
      // return a ref to const segment. However, since no segment exists
      // in path, this has to be passed by value. A solution is to provide
      // path with one front and one back segment in addition to the points.
    };


//   template<class P>
//     inline envelope<typename P::first_type, typename P::second_type>
//     make_envelope(const path<P>& pa)
//     {
//       for_each(pa.begin(), pa.end(), 
// 	       Envelope<typename P::first_type, typename P::second_type>());
//       envelope<typename P::first_type, typename P::second_type> e
// 	(min(fx,bx), min(fy,by), max(fx,bx), max(fy,by));
//       return e;
//     }


  typedef path<pointD> pathD;
  typedef path<pointI> pathI;
} //mb

template<class P> inline std::ostream&
operator<<(std::ostream& os, const mb::path<P>& pa) {
  return mb::print(pa.begin(), pa.end(), os, ", ", "(", ")");
}

template<class P>
inline istream& operator>>(istream& is, path<P>& pa)
{
  char c = 0;
  is >> c;
  if (c != '(') return iosBadbit(is);
  P p;
  while ((c != ')') && (is >> p >> c))
    {
      pa.push_back(p);
      if (c != ',') return iosBadbit(is);
    }
  return is;
}

#endif //ifndef _PATH_H_
