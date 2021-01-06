#include <RcppArmadillo.h>
#include <math.h>
#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace arma;
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector colmeans(arma::mat X){
    arma::mat mn = arma::mean(X, 0);
    return Rcpp::wrap(mn);
}

// [[Rcpp::export]]
NumericVector colsums(arma::mat X){
    arma::mat sm = arma::sum(X);
    return Rcpp::wrap(sm);
}

// [[Rcpp::export]]
NumericVector colsd(arma::mat X){
    arma::mat sds = arma::stddev(X, 0, 0);
    return Rcpp::wrap(sds);
}
// [[Rcpp::export]]
List prcomp_cpp(arma::mat X,
            bool scale,
            bool center){
    arma::mat Xscale;
    arma::mat Xcenter;
    arma::mat Xz = X;
    if(center){
        Xcenter = Rcpp::as<arma::mat>(colmeans(Xz));
        Xz = Xz - arma::repmat(Xcenter, Xz.n_rows, 1);
    }
    
    if(scale){
        Xscale = arma::repmat(Rcpp::as<arma::mat>(colsd(Xz)), Xz.n_rows, 1);
        Xz = Xz / Xscale;
    }
    arma::mat U;
    arma::vec s;
    arma::mat V;
    arma::mat x;
    svd(U,s,V,Xz);
    x = Xz * V;
    return Rcpp::List::create(
        Rcpp::Named("loadings") = V,
        Rcpp::Named("scores") = x);
    
}