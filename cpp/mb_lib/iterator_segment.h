#ifndef _ITERATOR_SEGMENT_H_
#define _ITERATOR_SEGMENT_H_

#include "segment.h"

namespace mb
{
  // temporary implementation; later: implement __segment_iterator<>
  template<class CPI>
    class const_segment_iterator
    {
    public:
      typedef const_segment_iterator<CPI> self;
      typedef segment<typename CPI::value_type> value_type;
      typedef const value_type& reference;
      typedef const value_type* pointer;
      const_segment_iterator(const CPI& ifront)
	: ifront_(ifront), iback_(ifront), segment_(*ifront_, *(++iback_))
	//  {cout << "\ncsi" << endl << *ifront_ << endl << *iback_ << endl;}
      {}
  
      bool operator==(const self& rhs) const {return ifront_ == rhs.ifront_;}
      bool operator!=(const self& rhs) const {return ifront_ != rhs.ifront_;}

      reference operator*() const {updateSegment(); return segment_;}
      pointer operator->() const {return &(operator*());}

      self& operator++() {++ifront_; ++iback_; return *this;}
      self operator++(int) {self t = *this; ++(*this); return t;}

      self& operator--() {--ifront_; --iback_; return *this;}
      self operator--(int) {self t = *this; --(*this); return t;}

    private:
      CPI ifront_, iback_;
      mutable value_type segment_; // why not CPI::value_type?

      void updateSegment() const
      {segment_.front() = *ifront_; segment_.back() = *iback_;}
    };
} //mb

#endif //ifndef _CONST_ITERATOR_H_
