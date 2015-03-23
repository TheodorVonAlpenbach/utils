#ifndef _ENVELOPE_H_
#define _ENVELOPE_H_

#include "interval.h"
#include "point.h"

namespace mb
{
  template<class Tx, class Ty = Tx>
    class envelope
    {
    public:
      envelope() {}
      envelope(const interval<Tx>& xi, const interval<Ty>& yi)
	: xi_(xi), yi_(yi) {}
      envelope(const Tx xmin, const Ty ymin, const Tx xmax, const Ty ymax)
	: xi_(xmin, xmax), yi_(ymin, ymax) {}
      const interval<Tx>& xI() const {return xi_;}
      const interval<Ty>& yI() const {return yi_;}
      interval<Tx>& xI() {return xi_;}
      interval<Ty>& yI() {return yi_;}
      bool containsX(const Tx x) const {return xi_.contains(x);}
      bool containsY(const Ty y) const {return yi_.contains(y);}
    private:
      interval<Tx> xi_;
      interval<Ty> yi_;
    };

  typedef envelope<double> envelopeD;
  typedef envelope<int> envelopeI;
  typedef envelope<unsigned int> envelopeUI;

} //mb

template<class Tx, class Ty> inline std::ostream& 
operator<< (std::ostream& os, const mb::envelope<Tx,Ty>& e) {
  os << "[" << e.xI() << e.yI() << "]"; return os;
}

#endif //ifndef _ENVELOPE_H_

