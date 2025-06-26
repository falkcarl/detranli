#' Make synthetic data
#' 
#' Generate synthetic data for the i-th null hypothesis: that the i-th response vector 
#' is exchangeable.
#' 
#' This function accommodates multiple point-scales. 
#' Items are grouped by point-scale, and responses are permuted only within each group.
#' 
#' @param i the row index of the respondent of interest.
#' @param data the Likert-type data (matrix or dataframe), full sample.
#' @param pointscales vector of integers indicating how many Likert-type response categories there are for each item. 
#' Length must match the number of columns in \code{data}.
#' @param numperms an integer indicating how many permutations to generate.
#' @param type Create synthetic data by doing permutations (\code{"perm"}) or by
#' from a first-order Markov Chain (\code{"markovchain"}) with the same
#' transition probabilities as the row. Defaults to permutations.
#' 
#' @return The empirical null distribution, a matrix where the first row is the observed response vector.
#' 
#' @import markovchain
#' @noRd
makesynth = function(i, data, pointscales, numperms=200, type=c("perm","markovchain")) {
  
  type = match.arg(type)
  
  # prepare to do shuffling within pointscales
  obs = unname(as.matrix(data)[i,])
  # column index vs pointscale
  originalposition = (1:length(pointscales))[order(pointscales)]
  # shuffle within pointscale
  unique_pointscales = sort(unique(pointscales))
  

  if(type=="perm"){
    bypointscale = lapply(unique_pointscales, function(k) {
      obs_thispointscale = obs[which(pointscales==k)]
      do.call(rbind, replicate(numperms, {
        sample(obs_thispointscale)
      }, simplify=FALSE))
    })    
  } else if (type=="markovchain"){
    bypointscale = lapply(unique_pointscales, function(k) {
      
      obs_thispointscale = obs[which(pointscales == k)]
      
      mcfit = markovchainFit(factor(obs[which(pointscales == k)], levels=1:k),
                              sanitize=TRUE)
      mc = new("markovchain", states = colnames(mcfit$estimate@transitionMatrix),
                    transitionMatrix = mcfit$estimate@transitionMatrix)
      # simulate from chain
      do.call(rbind, replicate(numperms, {
        as.numeric(markovchainSequence(length(obs_thispointscale), mc))
      }, simplify = FALSE))
    })
  }

  synth_disordered = do.call(cbind, bypointscale)
  # get into correct order
  synth = synth_disordered[, order(originalposition)]
  # put together
  together = rbind(obs, synth)
  return(together)
}