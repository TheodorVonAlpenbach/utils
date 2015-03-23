#ifndef _MB_CONSTANTS_H
#define _MB_CONSTANTS_H

#include <cmath>

#ifdef _MSC_VER
#define M_PI        3.14159265358979323846
#endif


namespace mb
{
  const double mb_PI = M_PI;
  const double DEG2RAD = 2.0*mb_PI/360.0;
}

#endif //_MB_CONSTANTS_H
