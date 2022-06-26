#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int factorial(int n){
  if(n > 1)
    return n * factorial(n - 1);
  else
    return 1;
}

//' Kendall's tau correlation coeficient
//'
//'Computes Kendall's tau correlation coeficient through Rcpp.
//'
//' @param x numeric vector
//' @param y numeric vector
//' @return double
//' @examples
//' fasttau(c(1,2,3,4,5),c(2,1,4,3,6))
//' @export
// [[Rcpp::export]]
double fasttau(NumericVector x, NumericVector y) {
  int nc = 0;
  int nd = 0;
  int n = x.size();
  for(int i = 0; i < n; ++i){
    for(int j = i; j < n; ++j){
      if(j == i){
      }else{
        if(j <= n){
          if(x[i] < x[j] && y[i] < y[j]){
            nc = nc + 1;
          }else if(x[i] > x[j] && y[i] > y[j]){
            nc = nc + 1;
          }else {
            nd = nd + 1;
          }
        }
      }
    }
  }
  return (nc - nd) / ((n * (n - 1.0))/2.0);
}
