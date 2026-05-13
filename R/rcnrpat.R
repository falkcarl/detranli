#' Generate content-nonresponsive data using a pattern of response
#' 
#' Generate a Likert-type sample to mimic the case where a participant may use
#' some nonresponsive pattern to complete the items.
#' 
#' @inheritParams rcnrunif
#' @param type Pattern type. "updown", "endpoints", "step", "straightlining",
#'  "seesawing","steplining", and "extremeSeesawing". See details.
#' @param prob Probability that pattern will be broken for any given item.
#' @param nstep If "step" or "steplining", how many times a response is typically repeated.
#' 
#' @details
#' The goal of this function is to mimic the case where a participant is
#' nonresponsive and completes the survey using one of several patterns.
#' 
#' Note that there is some redundancy with some implemented patterns, and some
#' may be removed at a later date. e.g., "seesawing" and "steplining" are 
#' slightly more general than "updown" and "step", respectively.
#' 
#' With "updown", participants complete items in a zig-zag pattern. e.g.,
#' 3, 4, 5, 4, 3, 2, 1, 2, 3, 4, etc. A random category is chosen as a starting
#' point and if the survey switches between subscales with different numbers of
#' categories per item (as specified by \code{pointscales}), the pattern also
#' starts over again at a random location. Item responses are not independent in
#' this case as the response to one item is often dependent on the participant's
#' response to a previous item.
#' 
#' With "endpoints", responses randomly toggle between the lowest and highest
#' category.
#' 
#' With "step", responses are similar to "updown" but the same response may be
#' repeated multiple times. e.g., 111222333.
#' 
#' With "straightlining", responses are fixed to an identical category across all items.
#' The fixed category is drawn randomly from all possible responses.
#' 
#' With "seesawing", participants complete items using a pattern close to zig-zag,
#' but the recurring sequence is randomly selected from adjacent categories. e.g., 
#' 1, 2, 3, 2, 1, 2, 3, etc.; 4, 3, 4, 3, 4, 3, 4, etc.; 2, 3, 4, 5, 4, 3, 2, etc. 
#' Two random categories are chosen as starting and ending
#' points and if the survey switches between subscales with different numbers of
#' categories per item (as specified by \code{pointscales}), the pattern also
#' starts over again at new random starting and ending points. Item responses are 
#' not independent in this case as the response to one item is often 
#' dependent on the participant's response to a previous item.
#'  
#' With "steplining", participants complete items using a straightlining pattern
#' with moving anchors. i.e., they repeat one response category multiple times and then move to 
#' the next adjacent category. e.g., 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 1, 1, 1, etc.
#' Like seesawing, two random categories are chosen as starting and ending
#' points and if the survey switches between subscales with different numbers of
#' categories per item (as specified by \code{pointscales}), the pattern also
#' starts over again at new random starting and ending points. Item responses are 
#' not independent in this case as the response to one item is often 
#' dependent on the participant's response to a previous item.
#' 
#' 
#' With "extremeSeesawing", participants complete items using a pattern similar to
#' seesawing, but the recurring sequence consist of solely two endpoints of the scale.
#' e.g., 1, 5, 1, 5, 1, 5, etc.; 7, 1, 7, 1, 7, 1, etc. Other generation details are the same
#' as "seesawing".
#' 
#' @return The sample matrix.
#' 
#' @seealso \code{\link{sample}}, \code{\link{rcnrunif}}, \code{\link{rcnrbinom}},
#'  \code{\link{rcnrreverse}}, \code{\link{rcnrstring}}
#' 
#' @export
#' 
#' @examples 
#' set.seed(47)
#' 
#' # 20 participants, 10 5 category items, updown or zig-zag pattern
#' rcnrpat(20, rep(5, 10), type="updown")
#' 
#' # 20 participants, 10 5 category items, mostly endpoints
#' rcnrpat(20, rep(5, 10), type="endpoints")
#' 
#' # 20 participants, 10 5 category items, step pattern
#' rcnrpat(20, rep(5, 10), type="step", nstep=4)
#' 
#' 
rcnrpat = function(n, pointscales, type=c("updown","endpoints","step","straightlining","seesawing","steplining","extremeSeesawing"),
                   prob=.05, nstep=4) {
  
  type = match.arg(type)
  
  if(length(prob)>1 | !is.numeric(prob)){
    stop("prob should be a single numeric value")
  }
  if (prob < 0 | prob >= 1){
    stop("prob should be equal to or above 0 and below 1")
  }
  if(nstep < 1){
    stop("nstep should be an integer, 1 or greater")
  }
  if(type=="updown"){
    dat = t(sapply(1:n, function(i){
      tmp = sample(pointscales[1], 1)
      inc = 1
      for(j in 2:length(pointscales)){
        # get random response?
        if(sample(c(TRUE,FALSE), 1, prob = c(prob, 1-prob))){
          tmp = c(tmp, sample(pointscales[j], 1))
        } else if (pointscales[j] != pointscales[j-1]) { # change in pointscales?
          tmp = c(tmp, sample(pointscales[j], 1))
        } else { # continue pattern
          if(tmp[j-1] == pointscales[j]){ # reached top of scale
            inc = -1            
          } else if (tmp[j-1] == 1) { # reached bottom of scale
            inc = 1
          } 
          tmp = c(tmp, tmp[j-1]+inc)
        }
      }
      tmp
    }))
  } else if(type=="seesawing"){
    dat = t(sapply(1:n, function(i){
      tmp = sample(pointscales[1], 1)
      start = tmp
      inc = 1
      while(T){
        end = sample(1:pointscales[1],1)
        if (start!=end) break
      }
      for(j in 2:length(pointscales)){
        if (pointscales[j] != pointscales[j-1]) { # change in pointscales?
          start = sample(pointscales[j], 1)
          while(T){
            end = sample(1:pointscales[j],1)
            if (start!=end) break
          }
        } 
        if (start < end){ # from smaller to larger response category
          if(tmp[j-1] == end){ # reached ending category
            inc = -1            
          } else if (tmp[j-1] == start) { # reached starting category
            inc = 1
          } 
          tmp = c(tmp, tmp[j-1]+inc)
        } else{# from larger to smaller response category
          if(tmp[j-1] == end){ # reached ending category
            inc = 1            
          } else if (tmp[j-1] == start) { # reached starting category
            inc = -1
          } 
          tmp = c(tmp, tmp[j-1]+inc)
        }
      }
      for (t in 1: length(pointscales)){
        # get random response?
        if(sample(c(TRUE,FALSE), 1, prob = c(prob, 1-prob))){
          tmp[t] = sample(pointscales[t],1)
        }
      }
      tmp
    }))
  } else if (type=="straightlining"){
    dat = t(sapply(1:n, function(i){
      tmp = numeric()
      fixed.resp <- sample(1:pointscales[1],1)
      for(j in 1:length(pointscales)){
        # get random response?
        if(sample(c(TRUE,FALSE), 1, prob = c(prob, 1-prob))){
          tmp = c(tmp, sample(pointscales[j], 1))
        } else if (pointscales[j] != pointscales[j-1]){
          fixed.resp <- sample(1:pointscales[j],1)
          tmp = c(tmp, fixed.resp)
        } else {
          tmp = c(tmp, fixed.resp)
        }
      }
      tmp
    }))
  } else if (type=="endpoints"){
    dat = t(sapply(1:n, function(i){
      tmp = numeric()
      for(j in 1:length(pointscales)){
        # get random response?
        if(sample(c(TRUE,FALSE), 1, prob = c(prob, 1-prob))){
          tmp = c(tmp, sample(pointscales[j], 1))
        } else {
          tmp = c(tmp, sample(c(1,pointscales[j]), 1))
        }
      }
      tmp
    }))
  } else if (type=="step"){
    dat = t(sapply(1:n, function(i){
      tmp = sample(pointscales[1], 1)
      inc = 1
      nrep = 1
      for(j in 2:length(pointscales)){
        
        # get random response?
        if(sample(c(TRUE,FALSE), 1, prob = c(prob, 1-prob))){
          tmp = c(tmp, sample(pointscales[j], 1))
        } else if (pointscales[j] != pointscales[j-1]) { # change in pointscales?
          tmp = c(tmp, sample(pointscales[j], 1))
        } else { # continue pattern
          if(nrep < nstep){
            tmp = c(tmp, tmp[j-1]) # same response as last time
            nrep = nrep + 1
          } else {
            if(tmp[j-1] == pointscales[j]){ # reached top of scale
              inc = -1
              nrep = 1
            } 
            else if (tmp[j-1] == 1) { # reached bottom of scale
              inc = 1
              nrep = 1
            }
            tmp = c(tmp, tmp[j-1]+inc)
            nrep = 1
          }
        }
      }
      tmp
    }))
  } else if (type=="steplining"){
    dat = t(sapply(1:n, function(i){
      tmp = sample(pointscales[1], 1)
      start = tmp
      nrep = 1
      nstep = sample(2:nstep,1)
      while(T){
        end = sample(1:pointscales[1],1)
        if (start!=end) break
      }
      for(j in 2:length(pointscales)){
        if (pointscales[j] != pointscales[j-1]) { # change in pointscales? restart
          tmp = c(tmp, sample(pointscales[j], 1))
          start = tmp[j]
          while(T){
            end = sample(1:pointscales[j],1)
            if (start!=end) break
          }     
        } 
        if (start < end){ #from smaller to larger response category
          if(nrep < nstep){
            tmp = c(tmp, tmp[j-1]) # same response as last time
            nrep = nrep + 1
          } else {
            if(tmp[j-1] == end){ # reached ending category
              inc = -(end-start)
              nrep = 1
            } 
            else{
              inc = 1
            }
            tmp = c(tmp, tmp[j-1]+inc)
            nrep = 1
          }
        } else{
          if(nrep < nstep){ # from larger to smaller response category
            tmp = c(tmp, tmp[j-1]) # same response as last time
            nrep = nrep + 1
          } else {
            if(tmp[j-1] == end){ # reached ending category
              inc = -(end-start)
              nrep = 1
            } 
            else{
              inc = -1
            }
            tmp = c(tmp, tmp[j-1]+inc)
            nrep = 1
          }
        }        
      }
      for(t in 1:length(pointscales))
      { # get random response?
        if(sample(c(TRUE,FALSE), 1, prob = c(prob, 1-prob))){
          tmp[t] = sample(pointscales[t],1)
        }  
      }
      tmp
    }))
  }  else if (type=="extremeSeesawing"){
    dat = t(sapply(1:n, function(i){
      start = sample(c(1,pointscales[1]),1)
      if (start == 1){
        end = pointscales[2]
      } else {
        end = 1
      }
      tmp <- c(start,end)
      j = 3
      while(j <= length(pointscales)){
        if (pointscales[j] != pointscales[j-2]) { # change in pointscales?
          start = sample(c(1,pointscales[j]),1)
          if (start == 1){
            end = pointscales[j+1]
          } else {
            end = 1
          }
        } 
        if (length(tmp) >= length(pointscales)-1){
          tmp = c(tmp, start)
          j = j + 1
        }else{
          tmp = c(tmp, c(start,end))
          j = j + 2
        }
      }
      for (j in 1: length(pointscales)){
        # get random response?
        if(sample(c(TRUE,FALSE), 1, prob = c(prob, 1-prob))){
          tmp[j] = sample(pointscales[j],1)
        }
      }
      tmp
    }))
  } 
  return(dat)
}
