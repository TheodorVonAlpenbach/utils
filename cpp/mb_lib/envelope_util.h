#ifndef _ENVELOPE_UTIL_H
#define _ENVELOPE_UTIL_H

#include "envelope.h"
#include "path.h"
#include "mb_topology.h"
#include "mb_minmax.h"

namespace mb
{
  // make these template later
  intervalD extension(const intervalD& i, const double d)
    {return intervalD(i.min() - d, i.max() + d);}
  
  envelopeD extension(const envelopeD& e, const double d)
    {return envelopeD(extension(e.xI(), d), extension(e.yI(), d));}

  envelopeD extension(const envelopeD& e, const double d1, const double d2)
    {return envelopeD(extension(e.xI(), d1), extension(e.yI(), d2));}

  envelopeD makeEnvelope(const pointD& a, const pointD& b)
  {return envelopeD(mb::min(a.x(), b.x()), mb::min(a.y(), b.y()),
		    mb::max(a.x(), b.x()), mb::max(a.y(), b.y()));}

  envelopeD makeEnvelope(const pathD& pa)
    {
      envelopeD e;
      for (pathD::const_iterator pai = pa.begin(); pai != pa.end(); ++pai)
	e = mb::g_union(e, *pai);
      return e;
    }

} // mb

#endif //_ENVELOPE_UTIL_H
