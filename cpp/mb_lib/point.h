#ifndef _POINT_H_
#define _POINT_H_

#include <map>
#include "mb_math.h"

using std::pair;

namespace mb
{
  // later: template for projection
  template<class Tx = double, class Ty = Tx>
    class point : public pair<Tx, Ty> //derives ==, <
    {
    public:
      explicit point(const Tx x = Tx(), const Ty y = Ty())
        : pair<Tx, Ty>(x, y) {}
      Tx x() const {return first;}
      Ty y() const {return second;}
      Tx& x() {return first;}
      Ty& y() {return second;}
    };

  typedef point<double> pointD;
  typedef point<int> pointI;

  template<class Tx, class Ty>
    bool operator==(const point<Tx, Ty>& p1, const point<Tx, Ty>& p2)
    {return eq(p1.x(), p2.x()) && eq(p1.y(), p1.y());}

  template<class Tx, class Ty>
    bool operator!=(const point<Tx, Ty>& p1, const point<Tx, Ty>& p2)
    {return !(p1 == p2);}
}

template<class Tx, class Ty> inline std::ostream& 
operator<<(std::ostream& os, const mb::point<Tx, Ty>& p) {
  return os << '(' << p.x() << ", " << p.y() << ')';
}

template<class Tx, class Ty>
inline istream& operator>>(istream& is, mb::point<Tx,Ty>& p)
{
  Tx x;
  Ty y;
  char c = 0;
  is >> c;
  if (c == '(') // format is '(x, y)'
    {
      is >> x >> c;
      if (c == ',') is >> y >> c;
      if (c != ')') iosBadbit(is);
    }
  else // format is 'x y'
    {
      is.putback(c);
      is >> x >> y;
    }
  if (is) p = mb::point<Tx,Ty>(x, y);
  return is;
}

#endif //ifndef _POINT_H_
