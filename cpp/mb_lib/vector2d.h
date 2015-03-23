#ifndef _VECTOR_H_
#define _VECTOR_H_

#include "point.h"
#include "mb_math.h"
#include <ostream>
#include "segment.h"

namespace mb
{
  template<class P>  // now based on point, later maybe pair
    class vector2d
    {
      typedef vector2d<P> self;
    public:
      vector2d(const P& p) : p_(p) {}
      vector2d(const P& p1, const P& p2)
	: p_(p2.x() - p1.x(), p2.y() - p1.y()) {}
      vector2d(const segment<P>& s)
	: p_(s.back().x() - s.front().x(), s.back().y() - s.front().y()) {}

      operator P() const {return p_;}
  
      double operator*(const self& rhs) const
      {return p_.x()*rhs.p_.x() + p_.y()*rhs.p_.y();}
  
      double cross(const self& rhs) const
      {return p_.x()*rhs.p_.y() - p_.y()*rhs.p_.x();}
  
      void operator*=(const double d) {p_.x() *= d; p_.y() *= d;}
      void operator+=(const self& rhs)
      {p_.x() += rhs.p_.x(); p_.y() += rhs.p_.y();}
      void operator-=(const self& rhs)
      {p_.x() -= rhs.p_.x(); p_.y() -= rhs.p_.y();}
      bool operator==(const self& rhs) {return p_ == rhs.p_;}

      std::ostream& print(std::ostream& os) const {return os << p_;}

    private:
      P p_; // point
    };

  typedef vector2d<pointD> vector2dD;

  template<class P>
    vector2d<P> operator*(const vector2d<P>& v, const double d)
    {vector2d<P> ret_v = v; ret_v *= d; return ret_v;}

  template<class P>
    vector2d<P> operator*(const double d, const vector2d<P>& v) {return v*d;}

  template<class P>
    vector2d<P> operator+(const vector2d<P>& lhs, const vector2d<P>& rhs)
    {vector2d<P> ret_v = lhs; ret_v += rhs; return ret_v;}

  template<class P>
    bool operator||(const vector2d<P>& lhs, const vector2d<P>& rhs)
    {return eq(lhs.cross(rhs), 0.0);}

  template<class P>
    vector2d<P> operator-(const vector2d<P>& v) {return -1.0*v;}

  template<class P> 
    double norm2(const vector2d<P>& v) {return v*v;}

  template<class P>
    double cos2_angle(const vector2d<P>& lhs, const vector2d<P>& rhs)
    { double normlhs = norm2(lhs);
      if (normlhs == 0) return 0;
      double normrhs = norm2(rhs);
      if (normrhs == 0) return 0;
      double prodv = lhs*rhs;
      if (prodv == 0) return 0;
      return sqr(prodv)/(normlhs*normrhs);}
      //return sqr(lhs*rhs)/(norm2(lhs)*norm2(rhs));}
}//mb

template<class P> inline std::ostream& 
operator<< (std::ostream& os, const mb::vector2d<P>& v) {return v.print(os);}

#endif //ifndef _VECTOR_H_
