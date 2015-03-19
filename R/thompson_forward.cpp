/* Estimate the forward part of the Thompson Recursion
 * as outlined in McCausland et al.
 * Given  List Omega_{tt},
 *        List c_{t}
 *      Matrix Omega_{t,t-1}
 * Return List   L = Cholesky(Sigma_{tt})
 *        List  LO = L\Omega_{t,t-1}
 *        vector m = Sigma_{tt} \ (c_t - Omega_{t,t-1} m_{t-1});
 */
// [[Rcpp::depends(RcppEigen)]]
#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/Core>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <Eigen/Eigen>

using namespace Rcpp;
using namespace RcppEigen;

using Eigen::VectorXd;
using Eigen::MatrixXd;
using Eigen::SparseMatrix;
using Eigen::MappedSparseMatrix;

typedef Eigen::Map<Eigen::MatrixXd>      MapMatd;
typedef Eigen::MappedSparseMatrix<double> MSpMat;
typedef Eigen::SparseMatrix<double>        SpMat;
typedef Eigen::SimplicialLLT<SpMat>       SpChol;

// [[Rcpp::export]]
List thompson_forward(List Ott, List cIn, MatrixXd Ot1) {
  int n = Ott.length();
  
  List Lambda(n);
  List LambdaOmega(n);
  List mOut(n);
  
  SEXP     s = Ott[0];
  SEXP     t = cIn[0];
  SpMat     Osp(as<SpMat>(s));
  VectorXd    c(as<VectorXd>(t));
  MatrixXd    O(Osp);
  MatrixXd    L  = O.llt().matrixL();
  MatrixXd    LO = L.triangularView<Eigen::Lower>().solve(Ot1);
  VectorXd     m = L.triangularView<Eigen::Lower>().solve(c);
               m = L.transpose().triangularView<Eigen::Upper>().solve(m); 
  Lambda[0]      = L;
  LambdaOmega[0] = LO;
  mOut[0]        = m;
  for(int i=1; i<n; i++){
//    MatrixXd LO = L.triangularView<Eigen::Lower>().solve(Ot1);
// That last line is unnecessary because I can recycle it from last time
    SEXP         s = Ott[i];
    SEXP         t = cIn[i];
    MatrixXd        O(as<SpMat>(s));
    VectorXd        c(as<VectorXd>(t));
    O             -= LO.transpose() * LO;
    L              = O.llt().matrixL();
    LO             = L.triangularView<Eigen::Lower>().solve(Ot1);
    c             -= Ot1 * m;
    m              = L.triangularView<Eigen::Lower>().solve(c);
    m              = L.transpose().triangularView<Eigen::Upper>().solve(m); 
    Lambda[i]      = L;
    LambdaOmega[i] = LO;
    mOut[i]        = m;
  }
  return(List::create(Named("Lambda")        = Lambda,
                      Named("LambdaOmega")   = LambdaOmega,
                      Named("m")             = mOut));

}

