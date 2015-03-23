/*****************************************************************************/
/*                                                                           */
/* (c) Copyright 1999 by                                                     */
/*     Flexim AS, Høvik, Norway                                              */
/*     All rights reserved. See the FiCopyright.h for more details.          */
/*                                                                           */
/*****************************************************************************/
#include <FiCopyright.h>

// $Id: geometries_basic.h,v 1.1.1.1 1999/11/17 14:33:10 ps Exp $

#ifndef _GEOMETRIES_BASIC_H_
#define _GEOMETRIES_BASIC_H_

template<class T = double>
class interval // closed, ie. [a, b]
{
public:
  interval(const interval<T>& i) : min_(i.min()), max_(i.max()) {}
  interval(const T min, const T max) : min_(min), max_(max) {}
  T min() const {return min_;}
  T max() const {return max_;}
  bool contains(const T c) const {return min_ <= c && c <= max_;}
private:
  T min_, max_;
};

template<class Tx = double, class Ty = Tx>
class envelope
{
public:
  envelope(const interval<Tx>& xi, const interval<Ty> yi)
    : xi_(xi), yi_(yi) {}
  envelope(const Tx xmin, const Ty ymin, const Tx xmax, const Ty ymax)
    : xi_(xmin, xmax), yi_(ymin, ymax) {}
  const interval<Tx>& xI() const {return xi_;}
  const interval<Ty>& yI() const {return yi_;}
  bool containsX(const Tx x) const {return xi_.contains(x);}
  bool containsY(const Ty y) const {return yi_.contains(y);}
private:
  interval<Tx> xi_;
  interval<Ty> yi_;
};

/*
Name:              "FiDbLoad"---
Syntax:            @FiDbLoad-syntax
Keywords:          feature format, GIS, parser, object structure, lex, yacc
Description:       Class "FiDbLoad" ...
Methods:
See also:          
Developed by:      Mats Bergstrøm, FlexIm AS, Høvik, Norway
Date:              99-06-11.
*/

#endif //ifndef _GEOMETRIES_BASIC_H_

// Version control
// $Log: geometries_basic.h,v $
// Revision 1.1.1.1  1999/11/17 14:33:10  ps
// First upload of mb_lib
//
