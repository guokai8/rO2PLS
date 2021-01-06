#include <Rcpp.h>
#include <math.h>
#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace std;
// [[Rcpp::export]]
List inilist(int n){
    List res(n);
    for(int i=0;i<n;i++){
        res[i]=i;
    }
    return(res);
}