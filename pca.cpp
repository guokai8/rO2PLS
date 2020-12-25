#include <RcppArmadillo.h>
#include <math.h>
#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]

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

// [[Rcpp::export(rng=false)]]
IntegerVector fast_table(IntegerVector x, int size = 5)
{
  IntegerVector tbl(size);

  for (IntegerVector::iterator it = x.begin(), end = x.end() ; it != end ; ++it)
  {
    if (*it <= size && *it > 0)
      tbl(*it-1)++;
  }

  return tbl;
}
// [[Rcpp::export(rng=false)]]
int fast_countequal(IntegerVector x, int t)
{
  return std::count(x.begin(), x.end(), t);
}

// [[Rcpp::export]]
int fast_countbelow(NumericVector x, double t)
{
  return std::count_if(x.begin(), x.end(), std::bind(std::less<double>(), std::placeholders::_1, t));
}

// [[Rcpp::export]]
int fast_countover(NumericVector x, double t)
{
  return std::count_if(x.begin(), x.end(), std::bind(std::greater<double>(), std::placeholders::_1, t));
}

// [[Rcpp::export(rng=false)]]
SEXP fast_eigen_values(arma::mat A)
{
  arma::mat coeff;
  arma::mat score;
  arma::vec latent;
  arma::princomp(coeff, score, latent, A);
  NumericMatrix eigenvalues = Rcpp::wrap(latent);
  NumericMatrix eigencoeff = Rcpp::wrap(coeff);
  return(List::create(_["eigen"] = eigenvalues, _["coeff"] = eigencoeff));
}

// [[Rcpp::export]]
List pca_cpp(arma::mat X,int ncomp,
             bool center,bool scale){
  arma::mat Mscale;
  arma::mat Mcenter;
  arma::mat scale_vec;
  arma::mat center_vec;
  arma::mat Xz = X;
  if(center){
    Mcenter = Rcpp::as<arma::mat>(colmeans(Xz));
    Xz = Xz - arma::repmat(Mcenter, Xz.n_rows, 1);
    center_vec = Mcenter.row(0);
  }

  if(scale){
    Mscale = arma::repmat(Rcpp::as<arma::mat>(colsd(Xz)), Xz.n_rows, 1);
    Xz = Xz / Mscale;
    scale_vec =  Mscale.row(0);
  }
  arma::mat coeff;
  arma::mat score;
  arma::vec latent;
  arma::princomp(coeff,score,latent,Xz);
  NumericMatrix loadings=Rcpp::wrap(coeff);
  NumericMatrix scores=Rcpp::wrap(score);
  NumericMatrix eigenvalues=Rcpp::wrap(latent);
  return Rcpp::List::create(
    Rcpp::Named("loadings") = loadings,
    Rcpp::Named("eigen_values")=eigenvalues,
    Rcpp::Named("scores") = scores);
}

