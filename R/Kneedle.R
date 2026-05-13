#' Careless responding detection using outlier statistics and the Kneedle algorithm
#' 
#' When using one index, the algorithm will output the cutoff value of this
#' specific index; when using multiple indices, it will output the cutoff 
#' value of the Mahanolabis Distance of all indices.
#' 
#'  
#' @param data Likert-type data (matrix or dataframe).
#' @param pointscales vector of integers indicating how many Likert-type response 
#' categories there are for each item. 
#' Length must match the number of columns in \code{data}.
#' @param leave_one_out Whether to use leave-one-out strategies when computing the outlier
#' statistics and the distance of these outlier statistics. Default is "both". See details.
#' @param feat_funs vector of nonresponsivity feature function names. Each function must 
#' return a numeric vector. Defaults to Mahalanobis distance and person-total
#' cosine similarity. See \code{"cnrdetect"}.
#' @param feat_idvals vector of ideal values, in the same order as \code{feat_funs}.
#' Defaults to 0 and +1 as the ideal points for Mahalanobis distance and person-total
#' cosine similarity.
#' @param custom_funs (Optional) named list containing name-function pairs for
#' user-defined feature functions. See \code{"cnrdetect"}.
#' @param missingmethod Method for handling missing data. See Details. Options are \code{"pointscalemidrange"} or \code{"EM"}. 
#' @param parallel_type (Experimental) If desired, the method of parallel processing. Each row's test
#' can be executed on a separate processing core. Options are \code{"lapply"} (default, no parallelization),
#' \code{"parallel"} (uses parallel package), \code{"furrr"} (uses furrr package). The latter two may
#' speed up computations considerably provided the computer has more than a couple processing cores to use.
#' @param ncores (Experimental/Optional) Number of processing cores to use if doing parallel processing.
#' @param seed (Optional) A random number seed, if you want the function to set the seed. Defaults to \code{NULL}.
#' @param plot (Optional) Whether to plot empirical CDF and the cutoff point. Defaults to \code{False}.

#' @details
#' 
#' Performs Kneedle algorithm for setting threshold (cutoff value) for any given outlier
#' statistics. 
#' 
#' When only one index is used in \code{feat_funs}, the algorithm will find thresholds for
#' the index (without reference to the ideal point as in \code{\link{cnrdetect}}).
#' The single index should flag respondents with higher values, so manual
#' transformation is needed before calling this function. This implementation
#' is to keep aligned with the original paper suggesting using Kneedle for
#' setting thresholds in detecting pattern respondents (Biemann et al., 2025).
#' Given the distribution of an index, we also use what we suspect is the same
#' underlying function from a `gen3dnet` package, \url{https://rdrr.io/github/MollahLab/gen3DNet/src/R/gen3DNet.R},
#' as used by Biemann et al. (2025) in order to determine the threshold. There
#' are other packages that could be used, however, to implement kneedle.
#' 
#' When multiple indices are used in \code{feat_funs}, the algorithm will find thresholds for
#' the indices with reference to the ideal points. Same as the main function (\code{\link{cnrdetect}}), 
#' the distance of all these indices. Some indices (e.g., "mahal" and "ptc") require reference 
#' samples for computation, and the distance measure also requires computing the covariance matrix of
#' indices based on a reference sample. Currently, two possible samples are provided
#' for both steps: using the whole sample (i.e., each individual has the same reference sample) 
#' or the leave-one-out sample (LOO, i.e., each individual has a different reference sample, which is all 
#' other respondents). The choice is controlled by \code{leave_one_out}. "Both" means that LOO is used 
#' for both steps; "indices" means that LOO is only used for computing relevant indices;
#' "distance" means that LOO is only used for computing distance; "none" means that
#' LOO is not used.
#' 
#' @references Biemann, T., Koch-Bayram, I. F., Meier-Barthold, M., & Aguinis, H.
#' (2025). Using Markov Chains to detect careless responding in survey research.
#' *Organizational Research Methods, 28*, 543-568. \url{https://doi.org/10.1177/10944281251334778}
#' 
#' @return Output is a list with the following elements:
#' 
#' \itemize{
#'  \item{\code{threshold}: the cutoff value used for making decisions.}
#'  \item{\code{distance}: the distance of indices for each individual (when multiple indices are used) 
#'  or the values of the index for each individual (when one index is used)}
#' }
#' 
#' 
#' @importFrom parallel makeCluster clusterSetRNGStream parLapply stopCluster
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multicore multisession sequential
#' @importFrom pbapply pblapply
#' @importFrom parallelly supportsMulticore
#' @importFrom stats setNames
#' @importFrom graphics abline text

#' @export
#' 
#' @examples
#' set.seed(4452)
#' 
#' # Obtain the thresholds and distance with Mahalanobis distance and person-total correlation
#' 
#' ## Without plots
#' detect.result = kneedle_distance(cnrexample1, pointscales=rep(5, times=ncol(cnrexample1)),
#' plot = FALSE) 
#' 
#' ## With plots
#' detect.result = kneedle_distance(cnrexample1, pointscales=rep(5, times=ncol(cnrexample1)),
#' plot = TRUE) 
#' 
#' # Make classification decisions
#' 
#' predicted.result <- detect.result$distance
#' predicted.label <- predicted.result
#' predicted.label[predicted.result<detect.result$threshold] = 0
#' predicted.label[predicted.result>=detect.result$threshold] = 1
#' 
#' # Obtain the thresholds with LazR
#' detect.lazR = kneedle_distance(cnrexample1, feat_funs=c("lazR"),
#' pointscales=rep(5, times=ncol(cnrexample1)), plot = TRUE) 

kneedle_distance = function(data, pointscales,leave_one_out=c("both","indices","distance","none"),
                            feat_funs=c("mahal", "ptcossim"), feat_idvals=c(0, +1),
                            custom_funs=NULL, missingmethod=c("pointscalemidrange","EM"),
                            parallel_type=c("lapply","parallel","furrr","pbapply"), ncores=NULL, seed=NULL,
                            plot=F){
  parallel_type=match.arg(parallel_type)
  missingmethod=match.arg(missingmethod)
  leave_one_out=match.arg(leave_one_out)
  
  if (missingmethod == "EM"){stop("EM has not been implemented yet for Kneedle.")}

  # Note: kneedlefx.R contains dist2d and kneedle.orm, which are modified from
  # https://rdrr.io/github/MollahLab/gen3DNet/src/R/gen3DNet.R
  # An attribution/credit appears in that R file.

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
  CRIsfun = function(i, data, pointscales, feats_combo, missingmethod,leave_one_out){
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
      if (leave_one_out == "both" || leave_one_out == "indices"){
        ref = as.matrix(data[-i,idx_nonmiss,drop=FALSE])
      }else{
        ref = as.matrix(data[,idx_nonmiss,drop=FALSE])
      }
      ref_completed = imputepsm(ref, pointscales[idx_nonmiss])
    } else {
      if (leave_one_out == "both" || leave_one_out == "indices"){
      ref_completed = as.matrix(data[-i, , drop=FALSE]) # don't fill in
      }else{
        ref_completed = as.matrix(data[, , drop=FALSE]) # don't fill in
      }
    }
    # feature space
    obs_nri = feats_combo(rbind(obs_likert), ref_completed, missargs=list(missingmethod=missingmethod, idx_nonmiss=idx_nonmiss))
    return(list(obs_nri = obs_nri))
  }
  
  # compute the distance with leave-one-out for covariance matrix
  distancefun = function(i, obs_nri,feat_idvals,leave_one_out){
    # feature space
    # the row/column of obs_nri when only one indicators is different from other scenarios, so I use && here ignoring the possibility of one sample dataset. Better solutions need to be used later.
    if (nrow(obs_nri)>1 && ncol(obs_nri)>1){
      if (leave_one_out == "both" || leave_one_out == "distance"){
      refdist = as.matrix(obs_nri[-i, , drop=FALSE]) 
      }else{
        refdist = as.matrix(obs_nri[, , drop=FALSE]) 
      }
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
    CRIslist = lapply(1:nrow(data), CRIsfun, data=data, pointscales=pointscales, 
                      leave_one_out=leave_one_out,feats_combo=feats_combo, missingmethod=missingmethod)
  } else if (parallel_type=="parallel") {
    cl = makeCluster(ncores)
    clusterSetRNGStream(cl, seed)
    CRIslist = parLapply(cl, as.list(1:nrow(data)), CRIsfun, data=data, pointscales=pointscales,
                         feats_combo=feats_combo, leave_one_out=leave_one_out,missingmethod=missingmethod)
    stopCluster(cl)
  } else if (parallel_type=="pbapply") {
    cl = makeCluster(ncores)
    clusterSetRNGStream(cl, seed)
    CRIslist =  pblapply(as.list(1:nrow(data)), CRIsfun, cl=cl,  data=data, pointscales=pointscales,
                         feats_combo=feats_combo, leave_one_out=leave_one_out, missingmethod=missingmethod)
    stopCluster(cl)
  } else if (parallel_type=="furrr"){
    if(supportsMulticore()){
      plan(multicore, workers=ncores)
    } else {
      plan(multisession, workers=ncores)
    }
    CRIslist = future_map(1:nrow(data),  CRIsfun, data=data, pointscales=pointscales,
                          feats_combo=feats_combo, leave_one_out=leave_one_out, missingmethod=missingmethod,
                          .options = furrr_options(seed=seed),
                          .progress=TRUE)
    #FIXME check giving me errors unless I do this
    if(!supportsMulticore()){
      plan(sequential)
    }
  }	
  obs_nri =  t(sapply(CRIslist, "[[", i="obs_nri"))
  
  # Compute distance then
  if(parallel_type=="lapply"){
    if(!is.null(seed)){
      set.seed(seed)
    }
    distancelist = lapply(1:nrow(data), distancefun, obs_nri=obs_nri,
                          leave_one_out=leave_one_out,feat_idvals=feat_idvals)
  } else if (parallel_type=="parallel") {
    cl = makeCluster(ncores)
    clusterSetRNGStream(cl, seed)
    distancelist = parLapply(cl, as.list(1:nrow(data)),  distancefun, 
                             leave_one_out=leave_one_out,obs_nri=obs_nri,feat_idvals=feat_idvals)
    stopCluster(cl)
  } else if (parallel_type=="pbapply") {
    cl = makeCluster(ncores)
    clusterSetRNGStream(cl, seed)
    distancelist =  pblapply(as.list(1:nrow(data)), distancefun, cl=cl,  
                             leave_one_out=leave_one_out,obs_nri=obs_nri,feat_idvals=feat_idvals)
    stopCluster(cl)
  } else if (parallel_type=="furrr"){
    if(supportsMulticore()){
      plan(multicore, workers=ncores)
    } else {
      plan(multisession, workers=ncores)
    }
    distancelist = future_map(1:nrow(data),  distancefun, 
                              leave_one_out=leave_one_out,obs_nri=obs_nri,feat_idvals=feat_idvals,
                          .options = furrr_options(seed=seed),
                          .progress=TRUE)
    #FIXME check giving me errors unless I do this
    if(!supportsMulticore()){
      plan(sequential)
    }
  }	
  distance =  sapply(distancelist, "[[", i="distance")
  
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
  list(threshold=distance.sort[index],distance=distance)
}
