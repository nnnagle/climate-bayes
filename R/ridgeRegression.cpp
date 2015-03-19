// [[Rcpp::depends(RcppEigen)]]
#include <Rcpp.h>
#include <RcppEigen.h>
using namespace Rcpp;
using namespace RcppEigen;

using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::Map;
using Eigen::HouseholderQR;
using Eigen::LLT;

// [[Rcpp::export]]
NumericVector ridgeRegression(MatrixXd X, MatrixXd K, VectorXd y) {
//  int           k  = X.cols();
  MatrixXd      Q  = X.transpose() * X;
                Q += K;
  MatrixXd      R  = Q.llt().matrixU();
  VectorXd    Xty  = X.transpose() * y;
  VectorXd     mu  = R.transpose().triangularView<Eigen::Lower>().solve(Xty);
               mu  = R.triangularView<Eigen::Upper>().solve(mu);
  NumericVector  e = rnorm(X.cols());
  // Move them into eigen
    VectorXd     z = as<VectorXd>(e);
    VectorXd     v = R.triangularView<Eigen::Upper>().solve(z);
    VectorXd     x = mu + v;
  return(wrap(x));
}
