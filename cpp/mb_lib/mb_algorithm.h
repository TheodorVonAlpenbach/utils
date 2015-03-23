#ifndef __MBE_INTERNAL_ALGORITHM_H
#define __MBE_INTERNAL_ALGORITHM_H

#include <limits>
#include <algorithm>
#include <functional>
#include <iterator>
#include <mb_iterator.h>
#include <mb_functional_spec.h>

// namespace std {
//   template<class In, class Out, class Pred>
//   inline Out copy_if(In first, In last, Out res, Pred p) {
//     while (first != last) {
//       if (p(*first))
// 	*res++ = *first;
//       ++first;
//     }
//     return res;
//   }
// }

namespace mb {
  template<class In, class Out, class Pred>
  inline Out copy_while(In first, In last, Out res, const Pred& p) {
    while (first != last && p(*first)) 
      *res++ = *first++;
    return res;
  }

  template<class In, class Out, class Pred>
  inline Out copy_until(In first, In last, Out res, const Pred& p) {
    return copy_while(first, last, res, std::not1(p));
  } 

  // Begin FB
  template<class In, class Out, class Pred, class Op>
  Out transform_if (In first, In last, Out res, Pred p, Op op) {
    while (first!=last) if(p(*first)) *res++ = op(*first++); else ++first;
    return res;
  }

  template<class C, class Op, class Res>
  void transform_in(C c, Op op, Res& res) {
    std::transform(c.begin(), c.end(), std::back_inserter(res), op);
  }

  template<class Op, class T, class Pred, class C>
  std::vector<T> transform_in_if(C c, Pred p, Op op) {
    std::vector<T> ret;
    transform_if(c.begin(), c.end(), std::back_inserter(ret), p, op);
    return ret;
  }

  template<class Op, class Arg> 
  inline std::vector<typename Op::result_type> 
  transform_vector (const std::vector<Arg>& arg, Op op) {
    typedef std::vector<typename Op::result_type> Res;
    std::vector<typename Op::result_type> ret;
    transform_in(arg, op, ret);
    return ret;
  }

  template<class Op>
  class Obj_ref : public std::unary_function<typename Op::argument_type, typename Op::result_type> {
  protected:
    Op& op_;
  public:
    Obj_ref(Op& op) : op_(op) {}
    typename Op::result_type operator()(const typename Op::argument_type& x) const { return op_(x); }
  };

  template<class Op> Obj_ref<Op> obj_ref(Op& op) { return Obj_ref<Op>(op); }


  /*
    template<class Op>
    class Pair1st : public std::unary_function<typename Op::argument_type, typename Op::result_type>
    {
    protected:
    Op op_;
    public:
    Pair1st(const Op& op) : op_(op) {}
    template<class P> typename Op::result_type operator()(const P& p) const { return op_(p.first); }
    template<class P> typename Op::result_type operator()(P& p) { return op_(p.first); }
    };

    template<class OP>
    Pair1st<OP> pair1st(const OP& op) { return Pair1st<OP>(op); }

    template<class Op>
    class Pair2nd : public std::unary_function<typename Op::argument_type, typename Op::result_type>
    {
    protected:
    Op op_;
    public:
    Pair2nd(const Op& op) : op_(op) {}
    template<class P> typename Op::result_type operator()(const P& p) const { return op_(p.second); }
    template<class P> typename Op::result_type operator()(P& p) { return op_(p.second); }
    };

    template<class OP>
    Pair2nd<OP> pair2nd(const OP& op) { return Pair2nd<OP>(op); }

  

    #ifdef _MSC_VER
    template<class Iter>
    class Pair1st_iterator : public std::iterator<
    __ITERATOR_CATEGORY(Iter),
    __VALUE_TYPE(Iter),
    __DISTANCE_TYPE(Iter) >
    #else
    template<class Iter>
    class Pair1st_iterator : public std::iterator<
    std::iterator_traits<Iter>::iterator_category,
    std::iterator_traits<Iter>::value_type,
    std::iterator_traits<Iter>::difference_type,
    std::iterator_traits<Iter>::pointer,
    std::iterator_traits<Iter>::reference>
    #endif
    {
    public:
    typedef Iter iterator_type;
    typedef const typename Iter::value_type::first_type& reference;
    typedef const typename Iter::value_type::first_type* pointer;
    typedef int difference_type;
    protected:
    typedef Pair1st_iterator self;
    iterator_type i_;
    public:
    Pair1st_iterator() : i_() {}
    Pair1st_iterator(iterator_type i) : i_(i) {}
    template<class U> Pair1st_iterator(const Pair1st_iterator<U>& i) : i_(i) {}

    Iter base() const { return i_.base(); }

    reference operator*() const { return i_->first; }
    pointer operator->() const { return &i_->first; }

    reference operator[](difference_type n) const { return i_[n].first; }

    Pair1st_iterator& operator++() { ++i_; return *this; }
    Pair1st_iterator operator++(int) { Pair1st_iterator t = i_; ++i_; return t; }
    Pair1st_iterator& operator--() { --i_; return *this; }
    Pair1st_iterator operator--(int) { Pair1st_iterator t = i_; --i_; return t; }

    Pair1st_iterator operator+(difference_type n) const { return i_+=n; }
    Pair1st_iterator& operator+=(difference_type n) { return i_+=n; }
    Pair1st_iterator operator-(difference_type n) const { return i_-=n; }
    Pair1st_iterator& operator-=(difference_type n) { return i_-=n; }

    friend bool operator!=(const Pair1st_iterator& a, const Pair1st_iterator& b);
    friend bool operator==(const Pair1st_iterator& a, const Pair1st_iterator& b);
    };

    template<class T>
    bool operator!=(const Pair1st_iterator<T>& a, const Pair1st_iterator<T>& b)
    { return a.i_ != b.i_; }

    template<class T>
    bool operator==(const Pair1st_iterator<T>& a, const Pair1st_iterator<T>& b)
    { return a.i_ == b.i_; }

    template<class Iter>
    Pair1st_iterator<Iter> pair1st_iterator(const Iter& i)
    { return Pair1st_iterator<Iter>(i); }

    #ifdef _MSC_VER
    template<class Iter>
    class Pair2nd_iterator : public std::iterator<
    std::iterator_traits<Iter>::iterator_category,
    std::iterator_traits<Iter>::value_type,
    std::iterator_traits<Iter>::difference_type>
    #else
    template<class Iter>
    class Pair2nd_iterator : public std::iterator<
    std::iterator_traits<Iter>::iterator_category,
    std::iterator_traits<Iter>::value_type,
    std::iterator_traits<Iter>::difference_type,
    std::iterator_traits<Iter>::pointer,
    std::iterator_traits<Iter>::reference>
    #endif
    {
    public:
    typedef Iter iterator_type;
    typedef const Iter::value_type::first_type& reference;
    typedef const Iter::value_type::first_type* pointer;
    typedef int difference_type;
    protected:
    typedef Pair2nd_iterator self;
    iterator_type i_;
    public:
    Pair2nd_iterator() : i_() {}
    Pair2nd_iterator(iterator_type i) : i_(i) {}
    template<class U> Pair2nd_iterator(const Pair2nd_iterator<U>& i) : i_(i) {}

    Iter base() const { return i_.base(); }

    reference operator*() const { return i_->second; }
    pointer operator->() const { return &i_->second; }

    reference operator[](difference_type n) const { return i_[n].second; }

    Pair2nd_iterator& operator++() { ++i_; return *this; }
    Pair2nd_iterator operator++(int) { Pair2nd_iterator t = i_; ++i_; return t; }
    Pair2nd_iterator& operator--() { --i_; return *this; }
    Pair2nd_iterator operator--(int) { Pair2nd_iterator t = i_; --i_; return t; }

    Pair2nd_iterator operator+(difference_type n) const { return i_+=n; }
    Pair2nd_iterator& operator+=(difference_type n) { return i_+=n; }
    Pair2nd_iterator operator-(difference_type n) const { return i_-=n; }
    Pair2nd_iterator& operator-=(difference_type n) { return i_-=n; }

    friend bool operator!=(const Pair2nd_iterator& a, const Pair2nd_iterator& b);
    friend bool operator==(const Pair2nd_iterator& a, const Pair2nd_iterator& b);
    };

    template<class T>
    bool operator!=(const Pair2nd_iterator<T>& a, const Pair2nd_iterator<T>& b)
    { return a.i_ != b.i_; }

    template<class T>
    bool operator==(const Pair2nd_iterator<T>& a, const Pair2nd_iterator<T>& b)
    { return a.i_ == b.i_; }

    template<class Iter>
    Pair2nd_iterator<Iter> pair2nd_iterator(const Iter& i)
    { return Pair2nd_iterator<Iter>(i); }
    // End FB
  */  

  // Position TODO: add :key to this; also add position_in, position_if<>
  template <class In, class T>
  size_t position(In first, In last, T x, size_t pos = 0) {
    for ( ; first != last; ++first, ++pos)
      if (*first == x) return pos;
    return -1;
  }

  template <class C, class T>
  size_t position_in(const C& c, T x, size_t pos = 0) { return position(c.begin(), c.end(), x, pos); }

  // For each additions
  template<class C, class Op>
  inline Op for_each_in(const C& c, Op f)
  {return std::for_each(c.begin(), c.end(), f);}

  template<class C, class Op>
  inline Op for_each_in(C& c, Op f)
  {return std::for_each(c.begin(), c.end(), f);}

  template <class In, class Op, class Pred>
  Op for_each_if(In first, In last, Op f, Pred p) {
    for ( ; first != last; ++first)
      if (p(*first))
	f(*first);
    return f;
  }

  template<class C, class Op, class Pred>
  inline Op for_each_in_if(const C& c, Op f, Pred p)
  {return for_each_if(c.begin(), c.end(), f, p);}

  template<class C, class Op, class Pred>
  inline Op for_each_in_if(C& c, Op f, Pred p)
  {return for_each_if(c.begin(), c.end(), f, p);}

  template <class Fwd, class Norm> //Norm: std::unary_function
  Fwd min_element_norm(Fwd first, Fwd last, const Norm& norm) {
    if (first == last) return first;

    typename Norm::result_type min = norm(*first);
    Fwd min_element = first++;
    while (first != last) {
      const double n = norm(*first);
      if (n < min) {
	min = n;
	min_element = first;
      }
      ++first;
    }
    return min_element;
  }

  template <class Fwd>
  Fwd min_element_norm(Fwd first, Fwd last) {
    return min_element_norm
      (first, last, std::identity<typename std::iterator_traits<Fwd>::value_type>());
  }

  // What's this? Seems useless to me... Commenting out this version
  // template <class C, class Norm>
  // void min_element_in(const C& c, Norm norm)
  // { return min_element_norm(c.begin(), c.end(), key); }

  // ... and inserting this one instead
  template <class C>
  void min_element_in(const C& c)
  { return min_element_norm(c.begin(), c.end()); }
   
  template <class BiDir, class Test, class Key>
  BiDir min_element_partition_recursive(BiDir first, BiDir last,
					const Test& less_, const Key& norm)
    // Puts minimum elements at front of [first, last] and returns
    // iterator to last front duplicate. Cannot be inlined because of
    // recursion. Maybe slow. Should provided less_ and norm_ with
    // good defaults (less<>, std::identity<> respectively).  The Test/Key
    // combination should also be applied for min_element_norm (with
    // an appropriate change of name).
  {
    if (first == last) return last; // [) -> [)r 
    BiDir curr = min_element_partition(mb::succ(first), last, less_, norm);
    --curr; // [3 [2 2 3c 4)) -> [3 2 2c 3 4)
    if (less_(norm(*first), norm(*curr)))
      return ++first;  // [1 2 2c 3 4) -> [1 2r 2c 3 4)
    if (less_(norm(*curr), norm(*first))) 
      swap(*first, *curr); // [3 2 2c 3 4) -> [2 2 3rc 3 4)
    else // *first == *curr
      ++curr;  // [2 2 2c 3 4) -> [2 2 2 3cr 4)
    return curr;
  }
   
  template <class BiDir, class Test, class Key>
  BiDir min_element_partition(BiDir first, BiDir last,
			      const Test& less_, const Key& norm)
    // Puts minimum elements at front of [first, last) and returns
    // iterator to first non-front-duplicate, possibly last (if empty
    // or all elements are equal). Should proved less and norm
    // with good defaults (less<>, std::identity<> respectively). The
    // Test/Key combination should also be applied for
    // min_element_norm (with an appropriate change of name).
  {
    BiDir curr = first;
    if (first == last || first == ++curr) return last; // [a) -> [a)r
    BiDir end = curr;
    while (curr != last) {
      if (less_(norm(*curr), norm(*first))) // *first < *curr
	{ // [3 3 2ec 2) -> [2 3e 3 2c)
	  swap(*first, *curr);
	  end = first; ++end;
	}
      else if (!less_(norm(*first), norm(*curr))) // *first == *curr
	{ // [2 2ec) -> [2 2)ec,  [2 3e 2c 4) -> [2 2 3e 4c)
	  swap(*end, *curr);
	  ++end;
	}
      ++curr;
    }
    return end;
  }

  template <class BiDir, class Test>
  BiDir min_element_partition(BiDir first, BiDir last, const Test& less_)
  {return min_element_partition
      (first, last, less_, std::identity<typename std::iterator_traits<BiDir>::value_type>());}

  template <class BiDir>
  BiDir min_element_partition(BiDir first, BiDir last)
  {return min_element_partition
      (first, last, std::less<typename std::iterator_traits<BiDir>::value_type>());}

  template <class Fwd, class Key>
  double min_norm(Fwd first, Fwd last, Key norm = 1 /*std::identity<double>()*/,
		  double min = std::numeric_limits<double>::max()) {
    double n;
    while (first != last) {
      n = norm(*first);
      if (n < min) 
	min = n;
      ++first;
    }
    return min;
  }
   
  template <class Fwd, class Key>
  double accumulate_norm(Fwd first, Fwd last, 
			 Key norm = 1 /*std::identity<double>()*/,
			 double init = 0) {
    while (first != last) {
      init += norm(*first);
      ++first;
    }
    return init;
  }
   
  template<class C, class T>
  inline typename C::const_iterator find_in(const C& c, const T& x)
  {return find(c.begin(), c.end(), x);}

  template<class C, class P>
  inline typename C::const_iterator find_in_if(const C& c, const P p)
  {return find_if(c.begin(), c.end(), p);}

  template<class C, class P>
  inline typename C::iterator find_in_if(C& c, const P p)
  {return find_if(c.begin(), c.end(), p);}

  template<class In, class Pred>
  inline bool every(In first, In last, const Pred& p) {
    // returns true if range is empty
    while (first != last) {
      if (!p(*first))
	return false;
      ++first;
    }
    return true;
  }

  template<class C, class Pred>
  inline bool every_in(const C& c, Pred p) 
  { return every(c.begin(), c.end(), p); }

  template<class In, class Pred>
  inline bool some(In first, In last, Pred p) 
  { return !every(first, last, std::not1(p)); }

  template<class C, class Pred>
  inline bool some_in(const C& c, Pred p)
  { return some(c.begin(), c.end(), p); }

  template<class In, class T>
  inline bool exists(In first, In last, const T& x) 
  { return some(first, last, equalTo(x)); }

  template<class C, class T>
  inline bool exists_in(const C& c, const T& x)
  {return exists(c.begin(), c.end(), x);}

  template <class In, class Op>
  inline typename Op::result_type
  average(In first, In last,
	  Op op = std::identity<typename std::iterator_traits<In>::value_type>()) {
    typename Op::result_type accum;
    if (first == last) return accum;
    int counter = 0;
    for (; first != last; ++first, ++counter)
      accum += op(*first);
    return 1.0*accum / counter;
  }

  template <class C, class Op = std::identity<typename C::value_type> >
  inline typename Op::result_type
  average_in (const C& c, Op op = Op()) 
  { return average(c.begin(), c.end(), op); }

  template <class T>
  inline T average2 (const T x, const T y) {return (x + y)/2;}

  template <class In, class R>
  inline R sum (In first, In last, R accum) {
    if (first == last) return accum;
    for (; first != last; ++first)
      accum += *first;
    return accum;
  }

  template <class C>
  inline double sum_in (C c)
  { return sum(c.begin(), c.end(), 0.0); }

  template <class C, class R>
  inline R sum_in (C c, R accum)
  { return sum(c.begin(), c.end(), accum); }

  template<class In>
  inline In delete_in(In first, In last) {
    while (first != last) {
      delete (*first);
      *first = 0;
      ++first;
    }
    return last;
  }

  template<class C>
  inline void delete_in(C& c) { delete_in(c.begin(), c.end()); }

} //mb

#endif //__MBE_INTERNAL_ALGORITHM_H
