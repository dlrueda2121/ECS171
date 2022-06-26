#' Kendall's Tau
#'
#'Compute Kendall's tau correlation coeficient between x and y,
#'where x and y have the same length.
#'
#' @param x numeric vector
#' @param y numeric vector
#'
#' @return double
#' @export
#' @examples
#' tau(c(1,2,3,4,5),c(2,1,4,3,6))
tau <- function(x,y) {
  nc <- 0
  nd <- 0
  for(i in 1:length(x)){
    for(j in i:length(x)){
      j <- j + 1
      if(j <= length(x)){
        if(x[i] < x[j] && y[i] < y[j]){
          nc = nc + 1
        }else if(x[i] > x[j] && y[i] > y[j]){
          nc = nc + 1
        }else {
          nd = nd + 1
        }
      }
    }

  }
  return((nc - nd) / choose(length(x),2))
}
