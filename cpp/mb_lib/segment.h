#ifndef _SEGMENT_H_
#define _SEGMENT_H_

#include "point.h"
#include "envelope.h"
#include "mb_minmax.h"

namespace mb
{
  template<class P = point<double> >
    class segment
    {
    public:
      segment(const P& front = P(), const P& back = P())
	: front_(front), back_(back) {}
      const P& front() const {return front_;}
      const P& back() const {return back_;}
      P& front() {return front_;}
      P& back() {return back_;}
    private:
      P front_, back_;
    };

  typedef segment<pointD> segmentD;
  typedef segment<pointI> segmentI;

  template<class P>
    inline double dx(const segment<P>& s) {return s.back().x() - s.front().x();}

  template<class P>
    inline double dy(const segment<P>& s) {return s.back().y() - s.front().y();}

  template<class P>
    inline envelope<typename P::first_type, typename P::second_type>
    envelopeOf(const segment<P>& s)
    {
      double fx = s.front().x();
      double fy = s.front().y();
      double bx = s.back().x();
      double by = s.back().y();
      envelope<typename P::first_type, typename P::second_type> e
	(min(fx,bx), min(fy,by), max(fx,bx), max(fy,by));
      return e;
    }
} //mb

template<class P> inline std::ostream& 
operator<<(std::ostream& os, const mb::segment<P>& s) {
  return os << '[' << s.front() << " " << s.back() << ']';
}

template<class P>
inline std::istream& operator>>(std::istream& is, segment<P>& s)
{ // format is '[p1 p2]'
  P p1, p2;
  char c = 0;
  is >> c;
  if (c != '[') return iosBadbit(is);
  is >> p1 >> p2 >> c;
  if (c != ']') return iosBadbit(is);
  s = segment<P>(p1, p2);
  return is;
}

#endif //ifndef _SEGMENT_H_
