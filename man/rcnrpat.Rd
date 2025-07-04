% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcnrpat.R
\name{rcnrpat}
\alias{rcnrpat}
\title{Generate content-nonresponsive data using a pattern of response}
\usage{
rcnrpat(
  n,
  pointscales,
  type = c("updown", "endpoints", "step"),
  prob = 0.05,
  nstep = 4
)
}
\arguments{
\item{n}{the sample size.}

\item{pointscales}{vector of integers indicating how many Likert-type response categories there are for each item. 
Length must match the number of columns in \code{data}.}

\item{type}{Pattern type. "updown", "endpoints", "step". See details.}

\item{prob}{Probability that pattern will be broken for any given item.}

\item{nstep}{If "step", how many times a response is typically repeated.}
}
\value{
The sample matrix.
}
\description{
Generate a Likert-type sample to mimic the case where a participant may use
some nonresponsive pattern to complete the items.
}
\details{
The goal of this function is to mimic the case where a participant is
nonresponsive and completes the survey using one of several patterns.

With "updown", participants complete items in a zig-zag pattern. e.g.,
3, 4, 5, 4, 3, 2, 1, 2, 3, 4, etc. A random category is chosen as a starting
point and if the survey switches between subscales with different numbers of
categories per item (as specified by \code{pointscales}), the pattern also
starts over again at a random location. Item responses are not independent in
this case as the response to one item is often dependent on the participant's
response to a previous item.

With "endpoints", responses randomly toggle between the lowest and highest
category.

With "step", responses are similar to "updown" but the same response may be
repeated multiple times. e.g., 111222333.

If \code{prob} is non-zero, \code{prob} of the time some other category is
chosen with uniform probability.

Likert-type categories are from 1 (not 0) to K where K is the highest category.
}
\examples{
set.seed(47)

# 20 participants, 10 5 category items, updown or zig-zag pattern
rcnrpat(20, rep(5, 10), type="updown")

# 20 participants, 10 5 category items, mostly endpoints
rcnrpat(20, rep(5, 10), type="endpoints")

# 20 participants, 10 5 category items, step pattern
rcnrpat(20, rep(5, 10), type="step", nstep=4)


}
\seealso{
\code{\link{sample}}, \code{\link{rcnrunif}}, \code{\link{rcnrbinom}},
 \code{\link{rcnrreverse}}, \code{\link{rcnrstring}}
}
