#' LazR
#' 
#' Calculates Lazy Respondent (LazR) statistic using first order Markov model.
#' 
#' @param x data (matrix or dataframe) of interest.
#' @param ref reference sample (matrix or dataframe). Must have same number of columns as \code{x}.
#' @param missargs (Experimental/Optional) A list of arguments for handling missing data. Currently
#'  a list of two elements \code{missingmethod} and \code{idx_nonmiss} are accepted. Details will
#'  later document how these work. This part is likely to change and only handles missing
#'  values in the reference sample.
#' 
#' @details
#' Computes LazR for each row in \code{x} based on the first order Markov model. 
#' Larger values of LazR are often thought to be
#' indicative of stronger patterns. Values for random/careful respondents are 0.2
#' (theoretical minimum, the ideal point) for a 5-category item.
#'  
#' @references Biemann, T., Koch-Bayram, I. F., Meier-Barthold, M., & Aguinis, H.
#' (2025). Using Markov Chains to detect careless responding in survey research.
#' *Organizational Research Methods, 28*, 543-568. \url{https://doi.org/10.1177/10944281251334778}
#' 
#' @return Vector of LazR values.
#' 
#' @importFrom markovchain markovchainFit createSequenceMatrix
#' 
#' @export
#'
#' @examples
#' x = cnrexample1[1:10,]
#' lazR(x) # LazR of each row
lazR <- function(x, ref, missargs = NULL, ...){
  sapply(1:nrow(x), function(i){
    mcfit <- markovchainFit(x[i,])
    sm <- createSequenceMatrix(unlist(x[i,]))
    sum(mcfit$estimate@transitionMatrix*sm)/(ncol(x)-1)
  })
}