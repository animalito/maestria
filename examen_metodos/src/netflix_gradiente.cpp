#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]

List netflix_gradiente(NumericVector i, NumericVector j, NumericVector x, 
                  NumericMatrix Pin, NumericMatrix Qin,
                  NumericVector a, NumericVector b, 
                  double gamma, double lambda) {
    NumericVector prow;
    NumericVector qrow;
    NumericMatrix Q = clone(Qin);
    NumericMatrix P = clone(Pin);
    NumericVector a1=clone(a);
    NumericVector b1=clone(b);
    double e;

    for(int t = 0; t < i.size(); t++){
         //Rcout << "Numero de usuario " << i(t) ;
         //Rcout << "Numero de pelicula " << j(t) << std::endl;
         e = x(t) - a1(i(t)-1) - b1(j(t)-1)- sum(P(i(t)-1,_)  * Q(j(t)-1,_) );
         prow = P(i(t)-1,_) + gamma*(e * Q(j(t)-1,_) - lambda* P(i(t)-1,_));
         qrow = Q(j(t)-1,_) + gamma*(e*P(i(t)-1,_) - lambda*Q(j(t)-1,_));
         P(i(t)-1,_) = prow;
         Q(j(t)-1,_) = qrow;
         a1(i(t)-1) = a1(i(t)-1) + gamma*(e-lambda*a1(i(t)-1));
         b1(j(t)-1) = b1(j(t)-1) + gamma*(e-lambda*b(j(t)-1));
    }
    return List::create(P,Q,a1,b1);
}
