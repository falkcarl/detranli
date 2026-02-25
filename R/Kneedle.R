#' Careless responding detection using outlier statistics and the Kneedle algorithm
#' 
#' When using one outlier statistic, the algorithm will output the cutoff value of this
#' specific statistic; when using multiple outlier statistics, it will output the cutoff 
#' value of the Mahanolabis Distance consisting of all outlier statistics.
#' 
#'  
#' @param data Likert-type data (matrix or dataframe).
#' @param pointscales vector of integers indicating how many Likert-type response 
#' categories there are for each item. 
#' Length must match the number of columns in \code{data}.
#' @param feat_funs vector of nonresponsivity feature function names. Each function must 
#' return a numeric vector. Defaults to Mahalanobis distance and person-total
#' cosine similarity. See Details.
#' @param feat_idvals vector of ideal values, in the same order as \code{feat_funs}.
#' Defaults to 0 and +1 as the ideal points for Mahalanobis distance and person-total
#' cosine similarity.
#' @param details If \code{FALSE} (the default), returns only p-values. If
#' \code{TRUE}, returns additional information in a list.
#' @param custom_funs (Optional) named list containing name-function pairs for
#' user-defined feature functions. See Details.
#' @param missingmethod Method for handling missing data. See Details. Options are \code{"pointscalemidrange"} or \code{"EM"}. 
#' @param parallel_type (Experimental) If desired, the method of parallel processing. Each row's test
#' can be executed on a separate processing core. Options are \code{"lapply"} (default, no parallelization),
#' \code{"parallel"} (uses parallel package), \code{"furrr"} (uses furrr package). The latter two may
#' speed up computations considerably provided the computer has more than a couple processing cores to use.
#' @param ncores (Experimental/Optional) Number of processing cores to use if doing parallel processing.
#' @param seed (Optional) A random number seed, if you want the function to set the seed. Defaults to \code{NULL}.
#' @param plot (Optional) Whether to plot empirical CDF and the cutoff point. Defaults to \code{False}.

#' @importFrom parallel makeCluster clusterSetRNGStream parLapply stopCluster
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multicore multisession sequential
#' @importFrom pbapply pblapply
#' @importFrom parallelly supportsMulticore
#' @importFrom stats setNames
#' @importFrom graphics abline text

#' @export
kneedle_distance = function(data, pointscales,
                            feat_funs=c("mahal", "ptcossim"), feat_idvals=c(0, +1),
                            details=FALSE, custom_funs=NULL, missingmethod=c("pointscalemidrange","EM"),
                            parallel_type=c("lapply","parallel","furrr","pbapply"), ncores=NULL, seed=NULL,plot=F){
  # Two functions used to perform the Kneedle algorithm, extracted from https://rdrr.io/github/MollahLab/gen3DNet/src/R/gen3DNet.R
  dist2d <- function(a,b,c) {
    v1 <- b - c
    v2 <- a - b
    m <- cbind(v1,v2)
    d <- det(m)/sqrt(sum(v1*v1))
    d
  }
  kneedle.orm <- function(values, sign) {
    start = c(1, values[1])
    end = c(length(values), values[length(values)])
    k <- which.max(lapply(1:length(values),
                          function(idx) {
                            sign * -1 * dist2d(c(idx, values[idx]),
                                               start,
                                               end
                            )
                          })
    )
    k
  }
  
  # Calculate multiple indicators
  feats_combo = function(x, ref, missargs) {
    sapply(feat_funs, function(f) {
      if(!is.null(custom_funs)){
        if(f %in% names(custom_funs)){
          f = custom_funs[[f]]
        } else {
          f = get(f)
        }
      } else {
        f = get(f)
      }
      f(x, ref, missargs=missargs)
    })
  }
  # get the distance of indicators of a row given reference
  feat2dist <- function(x, nulldist, feat_idvals) {
    # null variance matrix
    sig = tryCatch(stats::cov(nulldist, use="complete.obs"), 
                   error=function(u) {NULL})
    if(is.null(sig)) {
      return(NA)
    }
    if(any(is.na(sig))) {
      return(NA)
    }
    # null variance matrix inverse
    siginv = tryCatch(solve(sig), error=function(u) {NULL})
    if(is.null(siginv)) {
      return(NA)
    }
    # distance from ideal point
    reduced_obs = stats::mahalanobis(x, feat_idvals, siginv, inverted=TRUE)
    return(reduced_obs)
  }
  # compute the indicators for each row
  permfun = function(i, data, pointscales, feats_combo, missingmethod){
    # consider only nonmissing for a given row
    idx_nonmiss = which(!is.na(data[i,]))
    if(length(idx_nonmiss)<=1) {
      return(list(pval=NA, obs_nri=setNames(rep(NA, length(feat_funs)), feat_funs), synth_nri=NA,
                  synth_likert=NA))
    } # emergency exit: no more than one item nonmissing
    # likert space
    obs_likert = unname(data[i,idx_nonmiss])
    # missing data handling
    # fill in missing in anchor set
    if(missingmethod=="pointscalemidrange") {
      imputepsm <- function(x, pointscales) {
        completed = sapply(1:length(pointscales), function(j) {
          middle = (1+pointscales[j])/2
          ifelse(is.na(x[,j]), middle, x[,j])
        })
        return(completed)
      }
      
      ref = as.matrix(data[-i,idx_nonmiss,drop=FALSE])
      ref_completed = imputepsm(ref, pointscales[idx_nonmiss])
    } else {
      ref_completed = as.matrix(data[-i, , drop=FALSE]) # don't fill in
    }
    # feature space
    obs_nri = feats_combo(rbind(obs_likert), ref_completed, missargs=list(missingmethod=missingmethod, idx_nonmiss=idx_nonmiss))
    return(list(obs_nri = obs_nri))
  }
  
  # compute the distance with leave-one-out for covariance matrix
  permfun.distance = function(i, obs_nri,feat_idvals){
    # feature space
    # the row/column of obs_nri when only one indicators is different from other scenarios, so I use && here ignoring the possibility of one sample dataset. Better solutions need to be used later.
    if (nrow(obs_nri)>1 && ncol(obs_nri)>1){
      refdist = as.matrix(obs_nri[-i, , drop=FALSE]) 
      distance = feat2dist(obs_nri[i,], refdist, feat_idvals=feat_idvals)
    }
    # For single indicators, I do not use the reference and directly use the value of the indicator
    else{
      distance = obs_nri[,i]
    }
    # distance
    return(list(distance = distance))
  }
  # Compute indicators first
  if(parallel_type=="lapply"){
    if(!is.null(seed)){
      set.seed(seed)
    }
    permlist = lapply(1:nrow(data), permfun, data=data, pointscales=pointscales, feats_combo=feats_combo, missingmethod=missingmethod)
  } else if (parallel_type=="parallel") {
    cl = makeCluster(ncores)
    clusterSetRNGStream(cl, seed)
    permlist = parLapply(cl, as.list(1:nrow(data)), permfun, data=data, pointscales=pointscales,
                         feats_combo=feats_combo, missingmethod=missingmethod)
    stopCluster(cl)
  } else if (parallel_type=="pbapply") {
    cl = makeCluster(ncores)
    clusterSetRNGStream(cl, seed)
    permlist =  pblapply(as.list(1:nrow(data)), permfun, cl=cl,  data=data, pointscales=pointscales,
                         feats_combo=feats_combo, missingmethod=missingmethod)
    stopCluster(cl)
  } else if (parallel_type=="furrr"){
    if(supportsMulticore()){
      plan(multicore, workers=ncores)
    } else {
      plan(multisession, workers=ncores)
    }
    permlist = future_map(1:nrow(data),  permfun, data=data, pointscales=pointscales,
                          feats_combo=feats_combo, missingmethod=missingmethod,
                          .options = furrr_options(seed=seed),
                          .progress=TRUE)
    #FIXME check giving me errors unless I do this
    if(!supportsMulticore()){
      plan(sequential)
    }
  }	
  obs_nri =  t(sapply(permlist, "[[", i="obs_nri"))
  
  # Compute distance then
  if(parallel_type=="lapply"){
    if(!is.null(seed)){
      set.seed(seed)
    }
    permlist = lapply(1:nrow(data), permfun.distance, obs_nri=obs_nri,feat_idvals=feat_idvals)
  } else if (parallel_type=="parallel") {
    cl = makeCluster(ncores)
    clusterSetRNGStream(cl, seed)
    permlist = parLapply(cl, as.list(1:nrow(data)),  permfun.distance, obs_nri=obs_nri,feat_idvals=feat_idvals)
    stopCluster(cl)
  } else if (parallel_type=="pbapply") {
    cl = makeCluster(ncores)
    clusterSetRNGStream(cl, seed)
    permlist =  pblapply(as.list(1:nrow(data)), permfun.distance, cl=cl,  obs_nri=obs_nri,feat_idvals=feat_idvals)
    stopCluster(cl)
  } else if (parallel_type=="furrr"){
    if(supportsMulticore()){
      plan(multicore, workers=ncores)
    } else {
      plan(multisession, workers=ncores)
    }
    permlist = future_map(1:nrow(data),  permfun.distance, obs_nri=obs_nri,feat_idvals=feat_idvals,
                          .options = furrr_options(seed=seed),
                          .progress=TRUE)
    #FIXME check giving me errors unless I do this
    if(!supportsMulticore()){
      plan(sequential)
    }
  }	
  distance =  sapply(permlist, "[[", i="distance")
  
  # Kneedle algorithm
  distance.sort <- sort(distance)
  distance.sort.norm <- (distance.sort-min(distance,na.rm=T))/(max(distance,na.rm=T)-min(distance,na.rm=T))
  index <- kneedle.orm(distance.sort.norm,-1)
  # plot the cumulative distribution of the index
  if (plot){
    prob.sort <- 1:length(distance.sort.norm)
    prob.sort.norm <- prob.sort/max(prob.sort)
    plot(distance.sort.norm, prob.sort.norm, 
         xlab = "Normalized Indicator(s)", ylab = "Probability")
    abline(v = distance.sort.norm[index], col = "red", lwd = 2, lty = 2)
    text(x = distance.sort.norm[index], 
         y = prob.sort.norm[index], 
         labels = paste0("cutoff point = (", round(distance.sort[index], 3)," , ",round(prob.sort[index]/length(distance), 3),")"), 
         pos = 4, col = "red")
  }
  list(index=index,threshold=distance.sort[index],distance=distance,case.id = which.max(distance))
}
