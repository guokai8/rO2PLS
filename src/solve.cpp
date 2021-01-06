#include <RcppArmadillo.h>
#include <math.h>
#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace arma;
using namespace Rcpp;
// [[Rcpp::export]]
arma::mat solvec(arma::mat X){
    arma::mat x = arma::eye(X.n_cols,X.n_cols);
    arma::mat res = arma::solve(trans(X)*X,x);
    return(res);
}