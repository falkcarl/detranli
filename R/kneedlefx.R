# Two functions used to perform the Kneedle algorithm from
# https://rdrr.io/github/MollahLab/gen3DNet/src/R/gen3DNet.R
#
# License information for these when obtained in 2025 was as follows:

# MIT License
# Copyright (c) 2024 Mollah Lab
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

dist2d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  d <- det(m)/sqrt(sum(v1*v1))
  d
}

#' Find the knee/elbow point in a vector using the Kneedle algorithm.
#'
#' This function uses the Kneedle algorithm (Satopaa 2011)
#' to find the index of the knee point in the provided vector.
#' If the values are mostly increasing, use sign = 1. If they are
#' mostly decreasing, use sign = -1.
#'
#' @param values The values to find a knee/elbow in.
#' @param sign -1 if the values are mostly decreasing, 1 if the 
#' values are mostly decreasing.
#'
#' @return The index of the knee/elbow.
#'
#' @noRd
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