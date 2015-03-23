#ifndef _MATRIX_H
#define _MATRIX_H

#include <stdio.h>
#include <valarray>
#include <mb_math.h>
#include <mb_os.h>
#include <mb_is.h>
#include <string>
#include <mb_string_utils.h>

template<class T>
inline std::ostream& operator<<(std::ostream& os, const std::valarray<T>& va) {
  os << "[";
  for (int i = 0; i < va.size() - 1; ++i) { os << va[i] << " "; }
  os << va[va.size() - 1] << "]" << std::endl;
  return os;
}

inline std::ostream& operator<<(std::ostream& os, const std::slice& s) {
  os << "<" << s.start() << "," << s.size() << "," << s.stride() << ">";
  return os;
}

typedef std::pair<size_t, size_t> matrix_index;

class Dimensions : public matrix_index {
public:
  Dimensions(size_t n) : std::pair<size_t, size_t>(n, 1) {}
  Dimensions(size_t n, size_t m) : std::pair<size_t, size_t>(n, m) {}
  size_t size() const {return first*second;}
  size_t n() const {return first;}
  size_t m() const {return second;}
  Dimensions& operator+= (const Dimensions& x)
  { first += x.first; second += x.second; return *this; }
  Dimensions& operator- () 
  { first = -first; second = -second; return *this; }
};

inline Dimensions operator+ (const Dimensions& x, const Dimensions& y)
{ Dimensions ret = x; ret += y; return ret; }

inline Dimensions operator- (const Dimensions& mat) { Dimensions ret = mat; return -ret; }

inline Dimensions operator- (const Dimensions& x, const Dimensions& y)
{ return x + (-y); }

inline std::ostream& operator<<(std::ostream& os, const Dimensions& dim) {
  os << "[" << dim.n() << "x" << dim.m() << "]";
  return os;
}

template<class T>
class matrix
{
  typedef T value_type;
  typedef std::valarray<T> VA;

  size_t m_, n_;
  VA va_;
  bool is_transposed_;

public:
  matrix() : is_transposed_(false), va_(0) {}
  matrix(Dimensions dim) : m_(dim.m()), n_(dim.n()), va_(dim.size()), is_transposed_(false) {}
  matrix(int n, int m) : n_(n), m_(m), va_(n*m), is_transposed_(false) {}
  matrix(const VA& va, const size_t m) : m_(m), n_(va.size()/m), va_(va), is_transposed_(false) {}
  template <class In> matrix (In first, In last, size_t m) 
    : va_(std::distance(first, last)), m_(m), is_transposed_(false) {
    size_t size = va_.size();
    n_ = size/m_;
    if (size != n_*m_) std::cerr << "matrix(const std::valarray<T>)";
    for (int i = 0; first != last && i < size; ++first, ++i) {
      va_[i] = *first;
    }
  }
  matrix(const matrix& x)
    : m_(x.m_), n_(x.n_), va_(x.va_), is_transposed_(x.is_transposed_) {}

  matrix& operator= (const matrix& x)
  {m_ = x.m_; n_ = x.n_; va_ = x.va_; is_transposed_ = x.is_transposed_; return *this;}

  const VA& va() const {return va_;} // take care!
  VA& va() {return va_;}

  size_t num_rows() const {return is_transposed_ ? m_ : n_;}
  size_t num_cols() const {return is_transposed_ ? n_ : m_;}
  bool isTransposed() const {return is_transposed_;}

  // Returns column I of MAT
  std::slice rowSlice(size_t i, bool transpose = false) const
  {return isTransposed() == transpose ? std::slice(i, m_, n_) : std::slice(i*n_, n_, 1);}

  std::slice row(size_t i) const { return rowSlice(i, false); }
  std::slice col(size_t j) const { return rowSlice(j, true); }

  T operator() (const size_t i, const size_t j = 0) const 
  {return is_transposed_ ? va_[j*m_ + i] : va_[i*m_ + j];}
  T& operator() (const size_t i, const size_t j = 0)
  {return is_transposed_ ? va_[j*m_ + i] : va_[i*m_ + j];}

  matrix<T>& operator+= (const matrix<T>& m) {va_ += m.va_; return *this;}
  matrix<T>& operator*= (double c) {va_ *= c; return *this;}
  matrix<T>& transpose() { is_transposed_ = !is_transposed_; return *this; }
  matrix<T>& elt_mult(const matrix<T>& m) { va_ *= m.va_; return *this; }

  T elt_sum() const { return va_.sum(); }
};

template<class T>
inline matrix<T> operator+ (const matrix<T>& a, const matrix<T>& b) 
{ matrix<T> ret = a; return ret += b; }

template<class T> 
inline matrix<T> transpose(const matrix<T>& mat) { matrix<T> t = mat; return t.transpose(); }

template<class T> 
inline Dimensions dimensions(const matrix<T>& mat, int offset_i = 0, int offset_j = 0)
{return Dimensions(mat.num_rows() + offset_i, mat.num_cols() + offset_j);}

template<class T> 
inline T scalar_product(const std::valarray<T>& x, const std::valarray<T>& y)
{std::valarray<T> z = x*y; return z.sum();}

template<class T>
inline T& scalar_product(std::valarray<T>& x, const std::valarray<T>& y)
{x *= y; return x.sum();}

template<class T>
inline matrix<T> elt_mult(const matrix<T>& x1, const matrix<T>& x2) 
{ matrix<T> z = x1; z.elt_mult(x2); return z; }

template<class T>
inline matrix<T> elt_mult(const matrix<T>& x1, const matrix<T>& x2, 
			  const matrix<T>& x3) 
{ return elt_mult(x1, x2).elt_mult(x3); }

template<class T>
inline matrix<T> elt_mult(const matrix<T>& x1, const matrix<T>& x2,
			  const matrix<T>& x3, const matrix<T>& x4) 
{ return elt_mult(x1, x2, x3).elt_mult(x4); }

template<class T>
inline matrix<T> elt_mult(const matrix<T>& x1, const matrix<T>& x2,
			  const matrix<T>& x3, const matrix<T>& x4,
			  const matrix<T>& x5) 
{ return elt_mult(x1, x2, x3, x4).elt_mult(x5); }

template<class T>
inline void matrix_mult (const matrix<T>& a, const matrix<T>& b,
			 matrix<T>& res) {
  int an = a.num_rows();
  int am = a.num_cols();
  int bn = b.num_rows();
  int bm = b.num_cols();
  int rn = res.num_rows();
  int rm = res.num_cols();
  if (an != rn || am != bn || bm != rm) {
    fprintf(stderr, "matrix_mult: (%d,%d)*(%d,%d) != (%d,%d)\n", an,am,bn,bm,rn,rm);
    return;
  }
  // TODO: use copy_matrix instead?
  for (int i = 0; i < res.num_rows(); ++i)
    for (int j = 0; j < res.num_cols(); ++j) {
      const std::valarray<T>& v_a = a.va()[a.row(i)];
      const std::valarray<T>& v_b = b.va()[b.col(j)];
      res(i,j) = scalar_product(v_a, v_b);
    }
}

template<class T>
inline matrix<T> operator* (double c, const matrix<T>& mat) 
{ matrix<T> ret(mat); ret *= c; return ret; }

template<class T>
inline matrix<T> operator* (const matrix<T>& a, const matrix<T>& b) {
  matrix<T> ret(Dimensions(a.num_rows(), b.num_cols()));
  matrix_mult(a, b, ret);
  return ret;
}

template<class T>
inline size_t num_cols(const matrix<T>& mat, bool transpose = false) {
  bool transposed = mat.isTransposed() != transpose;
  return transposed ? mat.num_rows() : mat.num_cols();
}

// Norm methods
template<class T>
inline T average_matrix_norm (const matrix<T>& mat) 
{
  // Calculates the average absolute value of MAT's elements.
  const std::valarray<T>& v = abs(mat.va());
  return v.sum()/v.size();
}

template<class T>  // vec' * mat * vec -> R
inline double project_sym (const matrix<T>& vec, const matrix<T>& mat)
{ return (transpose(vec)*mat*vec)(0,0); }

template<class T>  // vec' * mat * vec -> R
inline double project_sym2 (const matrix<T>& vec, const matrix<T>& mat)
{
  const matrix<T>& vecT = transpose(vec);
  //    std::cout << "vecT: " << vecT << std::endl;
  //    std::cout << "mat: " << mat << std::endl;
  //    std::cout << "vec: " << vec << std::endl;
  //    std::cout << "mat*vec: " << mat*vec << std::endl;
  //    std::cout << "vecT*mat: " << vecT*mat << std::endl;
  //    std::cout << "vecT*mat*vec: " << vecT*mat*vec << std::endl;
  return 0; 
}

template<class T>
inline T normalize (matrix<T>& mat) { mat/average_matrix_norm(mat); }

template<class T>
inline std::ostream& operator<<(std::ostream& os, const matrix<T>& mat) 
{
  for (int i = 0; i < mat.num_rows(); ++i) {
    os << "|";
    int nCol = mat.num_cols();
    for (int j = 0; j < nCol - 1; ++j)
      os << mat(i, j) << " ";
    os << mat(i, nCol - 1) << "|" << std::endl;
  }
  return os;
}

// TODO make template T of this, but how do I type cast?
inline std::istream& operator>> (std::istream& is, matrix<std::string>& mat) {
  typedef std::list<std::string> LS;
  typedef std::list<LS> LLS;
  LLS rows;
  LS row;
  while (is) {
    row = mb::readList(is, 0, '|', '|');
    std::cout << "row:\n" << row << std::endl;
    if (!row.empty())
      rows.push_back(row); 
  }
  if (rows.empty()) return is;
  mat = matrix<std::string>(rows.size(), rows.front().size());

  LLS::const_iterator ri = rows.begin();
  for (int i = 0; ri != rows.end(); ++ri, ++i) {
    LS::const_iterator ei = ri->begin();
    for (int j = 0; ei != ri->end(); ++j, ++ei)
      mat(i, j) = *ei;
  }
  return is;
}


template<class T, class Op>
inline matrix<T>& set_matrix(Op op, matrix<T>& ret) {
  for (int i = 0; i < ret.num_rows(); ++i)
    for (int j = 0; j < ret.num_cols(); ++j)
      ret(i,j) = op(i,j);
  return ret;
}

template<class Mat, class ColOp>
inline void transform_column(Mat& mat, ColOp col_op, size_t column = 0) {
  for (int i = 0; i < mat.num_rows(); ++i) { mat(i, column) = col_op(i); }
}

template<class Mat, class MatOp>
inline void transform_matrix(Mat& mat, MatOp mat_op, matrix_index offset = matrix_index(0,0)) {
  for (int i = 0; i < mat.num_rows(); ++i)
    for (int j = 0; j < mat.num_cols(); ++j)
      { mat(i, j) = mat_op(i + matrix_index.first, j + matrix_index.first); }
}

// TODO: Implement directly from transform_matrix
template<class MatOp>
inline matrix<typename MatOp::result_type>
copy_matrix(Dimensions dim, MatOp mat_op, matrix_index offset = matrix_index(0,0)) {
  // dim: dimensions of resulting matrix; offset: offset to matrix operator(*.*)
  matrix<MatOp::result_type> ret(dim.n(), dim.m());
  for (int i = 0; i < dim.n(); ++i)
    for (int j = 0; j < dim.m(); ++j) 
      {	ret(i,j) = mat_op(i + offset.first, j + offset.second); }
  return ret;
}

template<class T, class C>
inline matrix<T> copy_rows(const matrix<T>& mat, const C& c) {
  size_t num_rows = c.size();
  matrix<T> ret(num_rows, mat.num_cols());
  C::const_iterator ci = c.begin();
  for (int i = 0; ci != c.end(); ++ci, ++i) {
    for (size_t j = 0; j < mat.num_cols(); ++j) {
      ret(i, j) = mat(*ci, j);
    }
  }
  return ret;
}
  
template<class MatOp>
inline matrix<typename MatOp::result_type> copy_matrix(MatOp mat_op) // = bin_identity
{return copy_matrix(dimensions(mat_op.mat_), mat_op);}

template<class Op>
inline void do_matrix(Op op, size_t n, size_t m) {
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < m; ++j)
      op(i,j);
}

template<class Op> 
inline void do_matrix(Op op, size_t n) { do_matrix(op, n, n); }

template<class Op>
inline void do_matrix_ul(Op op, size_t n, int offset = 0) {
  int i = 0;
  for (; i < - offset; ++i)
    for (int j = 0; j < n; ++j)
      op(i,j);
  for (; i < n; ++i)
    for (int j = i + offset; j < n; ++j)
      op(i,j);
}

template<class Op>
inline void do_matrix_sym(Op op, size_t n, int offset = 0) {
  for (int i = 0; i < n; ++i)
    for (int j = i + offset; j < n; ++j) {
      op(i,j);
      op(j,i);
    }
}

// generalized copy
template <class T>
inline matrix<T> trim(matrix<T> mat, std::list<size_t> rows, std::list<size_t> cols) {
  matrix<T> ret(rows.size(), cols.size());
  std::list<size_t>::const_iterator cii, cij;
  size_t i, j;
  for (cii = rows.begin(), i = 0; cii != rows.end(); ++cii, ++i)
    for (cij = cols.begin(), j = 0; cij != cols.end(); ++cij, ++j)
      ret(i,j) = sm(*cii, *cij);
  return ret;
}

// Matrix functors
template <class R> 
struct matrix_function : std::binary_function<int, int, R> {};

template <class R>
struct column_function : std::unary_function<int, R> {};

template <class T>
struct matrix_id : public matrix_function<T> {
  const matrix<T>& mat_;
  matrix_id(const matrix<T>& mat) : mat_(mat) {}
  T operator() (int i, int j) const { return mat_(i,j); }
};

template <class T> matrix_id<T> matrixId (const matrix<T>& mat)
{ return matrix_id<T>(mat); }

template <class T>
struct Diagonal : matrix_function<T> {
  const matrix<T>& mat_;
  Diagonal(const matrix<T>& mat) : mat_(mat) {}
  T operator() (int i, int j) const {return mat_(i, i);}
};

template <class T>
inline Diagonal<T> diagonal (const matrix<T>& mat) { return Diagonal<T>(mat); }

// std::string specialisations
inline matrix<std::string> readStringMatrix(const std::string& filename) {
  int num_cols = mb::countWords(mb::readLineFile(filename));
  std::list<std::string> strings = mb::readStringsFile(filename);
  return matrix<std::string>(strings.begin(), strings.end(), num_cols);
}

template <class T>
struct vector2matrix_t : column_function<T> {
  const std::vector<T>& v_;
  vector2matrix_t(const std::vector<T>& v) : v_(v) {}
  T operator() (int i) const { return v_[i]; }
};

template <class T> 
vector2matrix_t<T> vector2matrix(const std::vector<T>& v) {return vector2matrix_t<T>(v);}

#endif //_MATRIX_H
