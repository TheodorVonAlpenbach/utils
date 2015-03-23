#ifndef _DISTANCE_H_
#define _DISTANCE_H_

#include "point.h"
#include "segment.h"
#include "path.h"
#include "envelope.h"
#include "interval.h"
#include "vector2d.h"
#include "mb_math.h"
#include "mb_algorithm.h"
#include "mb_msg.h"
#include "mb_disjoint.h"
#include <algorithm>
#include <functional>
#include <iostream>

namespace mb {
  // distance

  template<class G1, class G2>
    struct Distance_sqr : unary_function<G2, double>
    {
      const G1& g1_;
      Distance_sqr(const G1& g1) : g1_(g1) {}
      double operator()(const G2& g2) const {return distance_sqr(g1_, g2);}
    };

  template<class G1, class G2>
    struct Distance_sqr_p : unary_function<G2, double>
    {
      const G1& g1_;
      Distance_sqr_p(const G1& g1) : g1_(g1) {}
      double operator()(const G2* g2) const {return distance_sqr(g1_, *g2);}
    };

  template<class G1, class G2>
    inline double distance_sqr(const G1& g1, const G2& g2)
    {return distance_sqr(g2, g1);}

  template<class T>
    inline double distance_sqr(const point<T>& p1, const point<T>& p2)
    {return sqr(p1.x() - p2.x()) + sqr(p1.y() - p2.y());}

  template<class T, class P>
    inline double distance_sqr(const point<T>& p, const segment<P>& s)
    {
      vector2d<P> vs(s);
      vector2d<P> vp(s.front(), p);
      const double vs_ip_vp = vs*vp;
      if (vs_ip_vp <= 0)
	return distance_sqr(p, s.front());
      const double vs_ip_vs = vs*vs;
      if (vs_ip_vs <= vs_ip_vp) 
	return distance_sqr(p, s.back());
      else 
	return vp*vp - sqr(vs_ip_vp)/vs_ip_vs;
    }

  template<class T, class P>
    inline double distance_sqr(const point<T>& p, const path<P>& pa)
    {return min_norm(pa.beginS(), pa.endS(), Distance_sqr<P, segment<P> >(p));}

  template<class Tx, class Ty>
    inline double distance_sqr(const envelope<Tx,Ty>& e, const point<Tx,Ty>& p)
    {return sqr(distanceI(e.xI(), p.x())) + sqr(distanceI(e.yI(), p.y()));}

  template<class T>
    inline double distanceI(const interval<T>& i, const T t)
    {
      T d = i.min() - t;
      if (d > 0) return d;
      d = t - i.max();
      if (d > 0) return d;
      return 0;
    }

  template<class G1, class G2>
    inline double distance(const G1& g1, const G2& g2)
    {return sqrt(distance_sqr(g1, g2));}

  // projection
  template<class T, class P>
    inline point<T> projection(const point<T>& p, const segment<P>& s)
    {
      vector2d<P> vs(s);
      vector2d<P> vp(s.front(), p);
      const double vs_ip_vp = vs*vp;
//        cout << "p: " << p << endl;
//        cout << "s: " << s << endl;
//        cout << "s.front(): " << s.front() << endl;
//        cout << "vs: " << vs << endl;
//        cout << "vp: " << vp << endl;
//        cout << "vs_ip_vp: " << vs_ip_vp << endl;
      if (vs_ip_vp <= 0)
	return s.front();
      const double vs_ip_vs = vs*vs;
//      cout << "vs_ip_vs: " << vs_ip_vs << endl;
      if (vs_ip_vs <= vs_ip_vp) 
	return s.back();
      else 
      {
//  	cerr << "vs " << vs << endl
//  	     <<"vp "<<vp<<endl
//  	     <<" vs_ip_vp "<<vs_ip_vp<<" vs_ip_vs "<<vs_ip_vs<<endl
//  	     <<"vs*(vs_ip_vp/vs_ip_vs) "<<vs*(vs_ip_vp/vs_ip_vs)<<endl
//  	     <<"s.front() + vs*(vs_ip_vp/vs_ip_vs) "
//  	     <<vector2d<P>(s.front()) + vs*(vs_ip_vp/vs_ip_vs)<<endl;
	return vector2d<P>(s.front()) + vs*(vs_ip_vp/vs_ip_vs);
      }
    }

  template<class T, class P>
    inline point<T> projection(const point<T>& p, const path<P>& pa)
    {
      return projection(p, *min_element_norm(pa.beginS(), pa.endS(),
					     Distance_sqr<P, segment<P> >(p)));}

  template<class T, class P>
    inline path<P> subpath(const path<P>& pa, const point<T>& p1,
			   const point<T>& p2)
    {
      path<P>::const_s_iterator s1 
        = find_if(pa.beginS(), pa.endS(), not1(Disjoint<T,P>(p1)));
//        cerr << "found s1 " << *s1 << endl;
      path<P>::const_s_iterator s2
	      = find_if(s1, pa.endS(), not1(Disjoint<T,P>(p2)));
        //cerr << "found s2 " << *s2 << endl;
      if (s1 == pa.endS() || s2 == pa.endS())
      {
	Error("subpath<T,P>", "points not on path, returning all of path!");
	return pa;
      }

      path<P> r_pa(s1, s2); // appends range [s1, s2); none if s1==s2
      if (s1 == s2) // ie. r_pa is empty
      {
	r_pa.push_back(p1);
	r_pa.push_back(p2);
	return r_pa;
      }

      //  cerr << "ret path constructed!" << endl;
      r_pa.pop_front();
      //  cerr << "pop!" << endl;

      if (s1->back() != p1)
      {
	//    cerr << "s1b: " << s1->back() << "p1: " << p1 << endl
	//	 << "r_pa: " << r_pa << endl;
	r_pa.push_front(p1);
	//    cerr << "rpapushed" << endl;
      }
      //  cerr << "opt push!" << endl;
      r_pa.push_back(p2);
      //  cerr << "pushed back, ready for return!" << endl;

      return r_pa; // returned ok!
    }

  template<class G> struct Length : unary_function<G, double>
  {
    double operator()(const G& g) const {return length(g);}
  };

  //  length should return main point type Tx
  template<class T> inline double length(const interval<T>& i)
    {return i.max() - i.min();}

  template<class P> inline double length(const segment<P>& s)
    {return mb::distance(s.front(), s.back());}

  template<class P> inline double length(const path<P>& pa)
    {return accumulate_norm(pa.beginS(), pa.endS(), Length<segment<P> >());}

} //mb

#endif //ifndef _DISTANCE_H_
