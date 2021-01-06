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

double means(arma::mat X){
    NumericMatrix Xz=Rcpp::wrap(X);
    double mu = mean(Xz);
    return(mu);
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
arma::mat getEigenValues(arma::mat M) {
    int nc=M.n_cols;
    arma::mat eigvec;
    arma::vec eigval;
    arma::eig_sym(eigval, eigvec,M);
    return (eigvec.col(nc-1));
}

// [[Rcpp::export]]
arma::mat meantest(arma::mat X,arma::mat Y,int ncomp,bool center,bool scale){
    arma::mat Xcenter;
    arma::mat Ycenter;
    arma::mat Xscale;
    arma::mat Yscale;
    arma::mat Xz = X;
    arma::mat Yz = Y;
    int nobj = Xz.n_rows;
    int npred = Xz.n_cols;
    int nresp = Yz.n_cols;
    arma::mat V = arma::zeros(npred,ncomp);
    arma::mat R = arma::zeros(npred,ncomp);
    
    //Y loadings
    arma::mat tQ = arma::zeros(ncomp,nresp);
    arma::cube B = arma::zeros(npred, nresp, ncomp);
    //loading weights
    
    arma::mat P = arma::zeros(npred,ncomp);
    // scores
    arma::mat U = arma::zeros(nobj,ncomp);
    arma::mat TT = arma::zeros(nobj,ncomp);
    // t't
    if(center){
        Xcenter = Rcpp::as<arma::mat>(colmeans(Xz));
        Xz = Xz - arma::repmat(Xcenter, Xz.n_rows, 1);
        Ycenter = Rcpp::as<arma::mat>(colmeans(Yz));
        Yz = Yz - arma::repmat(Ycenter, Yz.n_rows, 1);
    }
    if(scale){
        Xscale = arma::repmat(Rcpp::as<arma::mat>(colsd(Xz)), Xz.n_rows, 1);
        Xz = Xz / Xscale;
        Yscale = arma::repmat(Rcpp::as<arma::mat>(colsd(Yz)), Xz.n_rows, 1);
        Yz = Yz / Yscale;
    }
    arma::mat S;
    arma::mat qa;
    arma::mat ra;
    arma::mat q;
    arma::mat ta;
    arma::mat pa;
    arma::mat va;
    arma::mat ua;
    arma::mat cr;
    arma::mat tnorm;
    S = trans(Xz)*Yz;
    for(int i=0; i<ncomp;i++){
        if(nresp==1){
            qa = 1;
        }else{
            if (nresp < npred) {
                qa = getEigenValues(trans(S)*S);
                
            } else {
                q = getEigenValues(S*trans(S));
                qa = trans(S) * q;
                cr = sqrt(trans(qa)*qa);
                qa = qa / arma::repmat(cr,qa.n_rows,1);
            }
        }
        ra = S * qa;
        ta = Xz * ra;

        if(center){
            arma::mat tmean = mean(ta);
            Rcout << tmean<<std::endl;
            ta = ta - arma::repmat(tmean,ta.n_rows,1);
        }
        U.col(i)=ta;
    }
    return(U);
}
        