#include <Rcpp.h>

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
Rcpp::NumericVector timesTwo(int n, int N) {
  Rcpp::NumericVector x = Rcpp::runif(n*N, -1.9, 2.0);
  Rcpp::NumericMatrix m(n, N);
  std::copy(x.begin(), x.end(), m.begin());
  Rcpp::NumericVector S(N);
  std::cout << m(_ , 1);
  for(int i = 0; i < N; ++i){
    S = Rcpp::cumsum(m( _ , i));
  }
  return m;
  }
