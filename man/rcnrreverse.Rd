% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcnrreverse.R
\name{rcnrreverse}
\alias{rcnrreverse}
\title{Generate responses from model where human misses reverse-worded items}
\usage{
rcnrreverse(data, pointscales, whichreverse, severity = length(whichreverse))
}
\arguments{
\item{data}{Existing "human" data matrix with which to mess up.}

\item{pointscales}{Vector of integers indicating how many Likert-type
response categories there are for each item.}

\item{whichreverse}{Vector of item indices for which items are most likely to
be carelessly responded to.}

\item{severity}{Integer specifying how many items in \code{whichreverse}
should be messed up for each respondent.}
}
\value{
The sample matrix.
}
\description{
Generate a Likert-type sample to mimic the case where a participant may
complete most items diligently, but may accidentally miss reverse-worded
items.
}
\details{
Some participants respond diligently to many items, but sometimes misread
some items, especially reverse-worded items. Respondents do not see a
negation and assume the item is phrased in a positive way, leading them to
respond on the wrong end of the Likert-type response scale.

The current approach requires input from a hypothetical human sample, which
in examples is illustrated by fitting an (M)IRT model to data and using
the estimated model parameters to generate hypothetical humans.

Given a vector of item indices (\code{whichreverse}) for problematic items
(e.g., reverse-worded), and an integer for how many of these items are
problematic for any given respondent (\code{severity}), this function will
reverse code such items and then return the sample data matrix.

Note that such responses violate assumptions of \code{\link{cnrdetect}};
Item responses are not content unresponsive and are not exchangeable. As such,
rates of detecting such carelessness are not expected to have high sensitivity.
To the extent that contamination is low, it may be that such responders
are still flagged at higher rates than other diligent humans, however.
Further research on this topic is forthcoming.

Likert-type categories are from 1 (not 0) to K where K is the highest category.
}
\examples{
set.seed(47)

library(EFA.dimensions)
data(data_RSE)

# fit a mirt model to some data, theoretically unidimensional
# and with some reverse-worded items
library(mirt)
fit <- mirt(data_RSE, 1, "graded")
dat <- simdata(model=fit, N=10)

# funny that mirt has negative loadings for positively worded items
# and vice-versa. Doesn't matter though, leave as-is. Just means
# latent trait is in opposite direction. Model will still produce
# probabilities for response patterns similar to that in original data

dat <- dat + 1 # detranli expects indexing to start at 1, mirt starts at 0

# which items to sometimes reverse
whichreverse <- c(3, 5, 8, 9, 10)

# reverse code items
rcnrreverse(dat, pointscales = rep(5, 10), whichreverse, severity=4)


# fit a mirt model to multidimensional data w/ some reverse-worded items
library(psych)
data(bfi)
mod <- "F1 = 1-5
       F2 = 6-10
       F3 = 11-15
       F4 = 16-20
       F5 = 21-25
       COV = F1*F2*F3*F4*F5
       "
fit <- mirt(bfi[,1:25], mod, itemtype="graded",
            method="MHRM", draws=5)

dat <- simdata(model=fit, N=10)

dat <- dat + 1 # detranli expects indexing to start at 1, mirt starts at 0

# which items to sometimes reverse;
# guessing here based on inspecting coefficients from fitted model
# may require some content knowledge regarding the items
whichreverse <- c(1, 9, 10, 11, 12, 22, 25)

# reverse code items
rcnrreverse(dat, pointscales = rep(6, 25), whichreverse, severity=7)

}
\seealso{
\code{\link{sample}}, \code{\link{rcnrunif}}, \code{\link{rcnrbinom}},
 \code{\link{rcnrstring}}, \code{\link{rcnrpat}}
}
