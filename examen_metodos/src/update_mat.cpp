#include <Rcpp.h>
using namespace Rcpp;
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// [[Rcpp::export]]
int update_mat(NumericMatrix x, int doc_no, NumericVector h) {
   int nrow = x.nrow();
   int ncol = x.ncol();
   for(int i =0; i < nrow; i++){
       if(h[i] < x( i,doc_no - 1)){
            x(i,doc_no - 1) =  h[i];
       }
   }
   return 1;
}
