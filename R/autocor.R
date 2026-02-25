#' Autocor
#' 
#' Calculates Auto-Correlations (Autocor) statistic.
#' 
#' @param x data (matrix or dataframe) of interest.
#' @param ref reference sample (matrix or dataframe). Must have same number of columns as \code{x}.
#' @param whichone The algorithm used for pooling the results from multiple ns in computing 
#' lag-n autocorrelations
#' @param missargs (Experimental/Optional) A list of arguments for handling missing data. Currently
#'  a list of two elements \code{missingmethod} and \code{idx_nonmiss} are accepted. Details will
#'  later document how these work. This part is likely to change and only handles missing
#'  values in the reference sample.
#' 
#' @details
#' Computes Autocor for each row in \code{x}. 
#' Larger values of Autocor are often thought to be
#' indicative of stronger patterns. Values for random/careful respondents are 0
#'  (theoretical minimum, the ideal point).
#' 
#' @return Vector of LazR.
#' 
#' 
#' @importFrom responsePatterns rp.acors rp.indices
#' 
#' @export
#'
#' @examples
#' x = cnrexample1[1:10,]
#' autocor(x) # LazR of each row
autocor <- function(x, ref, whichone = c("sum","max"),  missargs = NULL, ...){
  whichone <- match.arg(whichone)
  ac <- rp.acors(x, max.lag = 5)
  #ac <- rp.acors(x) # having the full spectrum of lag takes longer and loses specificity?
  if(whichone=="max"){
    return(rp.indices(ac)$max.abs.ac)
  } else if (whichone=="sum"){
    return(rp.indices(ac)$sum.abs.ac)
  }
  
}