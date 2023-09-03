//
// Rcpp/C++ implementation of fast random transforms based on complex FFT
//

// needs RcppArmadillo for FFT (Rcpp.h will be automatically included)
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;

// ~~~~~~~~~~~~~~~
// ~ FFT1 method ~
// ~~~~~~~~~~~~~~~
// rotate -> FFT -> rotate

//' Applies forward FFT1-type fort transform
//'
//' @param X_Re numeric matrix for real component of input
//' @param X_Im numeric matrix for imaginary component of input
//' @param s1 numeric vector for real component of sc1
//' @param s2 numeric vector for imaginary component of sc1
//' @param s3 numeric vector for real component of sc2
//' @param s4 numeric vector for imaginary component of sc2
//' @return transformed numeric matrix
//' @noRd
// [[Rcpp::export(.FORT_CPP_FFT1_fwd, rng = false)]]
arma::mat FORT_CPP_FFT1_fwd(const arma::mat& X_Re, const arma::mat& X_Im, const arma::vec& s1, const arma::vec& s2, const arma::vec& s3, const arma::vec& s4) {
  // this function performs the forward evaluation
  // set X to be a complex matrix using X_Re for real component and X_Im for imaginary component
  arma::cx_mat X(X_Re,X_Im);
  // allocate vectors for complex scaling
  arma::cx_vec sc1(s1,s2);
  arma::cx_vec sc2(s3,s4);
  // rotate/scale
  X.each_col() %= sc1;
  // fft
  X = arma::fft(X);
  // rotate/scale
  X.each_col() %= sc2;
  // convert to real and concatenate
  return arma::join_vert(arma::real(X),arma::imag(X));
}

//' Applies reverse FFT1-type fort transform
//'
//' @param X_Re numeric matrix for real component of input
//' @param X_Im numeric matrix for imaginary component of input
//' @param s1 numeric vector for real component of sc2i
//' @param s2 numeric vector for imaginary component of sc2i
//' @param s3 numeric vector for real component of sc1i
//' @param s4 numeric vector for imaginary component of sc1i
//' @return transformed numeric matrix
//' @noRd
// [[Rcpp::export(.FORT_CPP_FFT1_rev, rng = false)]]
arma::mat FORT_CPP_FFT1_rev(const arma::mat& X_Re, const arma::mat& X_Im, const arma::vec& s1, const arma::vec& s2, const arma::vec& s3, const arma::vec& s4) {
  // this function performs the reverse evaluation
  // set X to be a complex matrix using X_Re for real component and X_Im for imaginary component
  arma::cx_mat X(X_Re,X_Im);
  // allocate vectors for complex scaling
  arma::cx_vec sc2i(s1,s2);
  arma::cx_vec sc1i(s3,s4);
  // rotate/scale
  X.each_col() %= sc2i;
  // inverse fft
  X = arma::ifft(X);
  // rotate/scale
  X.each_col() %= sc1i;
  // convert to real and concatenate
  return arma::join_vert(arma::real(X),arma::imag(X));
}

// ~~~~~~~~~~~~~~~
// ~ FFT2 method ~
// ~~~~~~~~~~~~~~~
// rotate -> FFT -> rotate -> FFT -> rotate

//' Applies forward FFT2-type fort transform
//'
//' @param X_Re numeric matrix for real component of input
//' @param X_Im numeric matrix for imaginary component of input
//' @param s1 numeric vector for real component of sc1
//' @param s2 numeric vector for imaginary component of sc1
//' @param s3 numeric vector for real component of sc2
//' @param s4 numeric vector for imaginary component of sc2
//' @param s5 numeric vector for real component of sc3
//' @param s6 numeric vector for imaginary component of sc3
//' @return transformed numeric matrix
//' @noRd
// [[Rcpp::export(.FORT_CPP_FFT2_fwd, rng = false)]]
arma::mat FORT_CPP_FFT2_fwd(const arma::mat& X_Re, const arma::mat& X_Im, const arma::vec& s1, const arma::vec& s2, const arma::vec& s3, const arma::vec& s4, const arma::vec& s5, const arma::vec& s6) {
  // this function performs the forward evaluation
  // set X to be a complex matrix using X_Re for real component and X_Im for imaginary component
  arma::cx_mat X(X_Re,X_Im);
  // allocate vectors for complex scaling
  arma::cx_vec sc1(s1,s2);
  arma::cx_vec sc2(s3,s4);
  arma::cx_vec sc3(s5,s6);
  // rotate/scale
  X.each_col() %= sc1;
  // fft
  X = arma::fft(X);
  // rotate/scale
  X.each_col() %= sc2;
  // fft
  X = arma::fft(X);
  // rotate/scale
  X.each_col() %= sc3;
  // convert to real and concatenate
  return arma::join_vert(arma::real(X),arma::imag(X));
}

//' Applies reverse FFT2-type fort transform
//'
//' @param X_Re numeric matrix for real component of input
//' @param X_Im numeric matrix for imaginary component of input
//' @param s1 numeric vector for real component of sc3i
//' @param s2 numeric vector for imaginary component of sc3i
//' @param s3 numeric vector for real component of sc2i
//' @param s4 numeric vector for imaginary component of sc2i
//' @param s5 numeric vector for real component of sc1i
//' @param s6 numeric vector for imaginary component of sc1i
//' @return transformed numeric matrix
//' @noRd
// [[Rcpp::export(.FORT_CPP_FFT2_rev, rng = false)]]
arma::mat FORT_CPP_FFT2_rev(const arma::mat& X_Re, const arma::mat& X_Im, const arma::vec& s1, const arma::vec& s2, const arma::vec& s3, const arma::vec& s4, const arma::vec& s5, const arma::vec& s6) {
  // this function performs the reverse evaluation
  // set X to be a complex matrix using X_Re for real component and X_Im for imaginary component
  arma::cx_mat X(X_Re,X_Im);
  // allocate vectors for complex scaling
  arma::cx_vec sc3i(s1,s2);
  arma::cx_vec sc2i(s3,s4);
  arma::cx_vec sc1i(s5,s6);
  // rotate/scale
  X.each_col() %= sc3i;
  // inverse fft
  X = arma::ifft(X);
  // rotate/scale
  X.each_col() %= sc2i;
  // inverse fft
  X = arma::ifft(X);
  // rotate/scale
  X.each_col() %= sc1i;
  // convert to real and concatenate
  return arma::join_vert(arma::real(X),arma::imag(X));
}
