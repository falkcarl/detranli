#' Autocor
#' 
#' Calculates Auto-Correlation (Autocor) statistic.
#' 
#' @param x data (matrix or dataframe) of interest.
#' @param ref reference sample (matrix or dataframe). Must have same number of columns as \code{x}.
#' @param whichone The algorithm used for pooling the results from multiple lags in computing 
#' lag-n autocorrelations
#' @param max.lag The maximum lag considered.
#' @param missargs (Experimental/Optional) A list of arguments for handling missing data. Currently
#'  a list of two elements \code{missingmethod} and \code{idx_nonmiss} are accepted. Details will
#'  later document how these work. This part is likely to change and only handles missing
#'  values in the reference sample.
#' 
#' @details
#' Computes Autocor for each row in \code{x}.
#' 
#' For a row of data, the algorithm will compute all autocorrelations up to the
#' maximum lag specified by the function. This will result in multiple autocorrelations
#' for a single row of data. These are then either summed together (default) or
#' the maximum is taken as the Autocor value for the single row.
#' 
#' Larger values of Autocor are often thought to be
#' indicative of stronger patterns. Values for random/careful respondents are 0
#' (theoretical minimum, the ideal point), though this would be expected to hold
#' only to the extent that the questionnaire design does not elicit a pattern.
#' 
#' @references Gottfried, J., Ježek, S., Králová, M., & Řiháček, T. (2022).
#' Autocorrelation Screening: A potentially efficient method for detecting
#' repetitive response patterns in questionnaire data. *Practical Assessment,
#' Research, and Evaluation, 27 (2)*. \url{https://doi.org/10.7275/vyxb-gt24}
#' 
#' @return Vector of AutoCor values.
#'  
#' @importFrom responsePatterns rp.acors rp.indices
#' 
#' @export
#'
#' @examples
#' x = cnrexample1[1:10,]
#' autocor(x) # LazR of each row
autocor <- function(x, ref, whichone = c("sum","max"), max.lag=5,
                    missargs = NULL, ...){
  whichone <- match.arg(whichone)
  ac <- rp.acors(x, max.lag = 5)
  if(whichone=="max"){
    return(rp.indices(ac)$max.abs.ac)
  } else if (whichone=="sum"){
    return(rp.indices(ac)$sum.abs.ac)
  }
  
}