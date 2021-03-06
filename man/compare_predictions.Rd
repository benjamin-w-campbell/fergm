% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_predictions.R
\name{compare_predictions}
\alias{compare_predictions}
\title{Compare predictions of ERGM to FERGM.}
\usage{
compare_predictions(ergm.fit = NULL, fergm.fit = NULL, seed = NULL,
  replications = 500)
}
\arguments{
\item{ergm.fit}{A model object returned by the \code{ergm} function.  Must be specified.}

\item{fergm.fit}{A model object returned by the \code{fergm} function.  Must be specified.}

\item{seed}{An integer that sets the seed for the random number generator to assist in replication.  Defaults to a null value for no seed setting.}

\item{replications}{The number of networks to be simulated to assess predictions. Defaults to 500.}
}
\value{
The compare_predictions function returns a matrix reflecting the number of correctly predicted ties for the ERGM and FERGM for each network simulated.
}
\description{
This function allows you to assess the importance of the frailty term in prediction by comparing the predictive accuracy of an ERGM to an FERGM.
Note: Prior to estimating this function, ensure the network object of interest is saved to the global environment and named "net."
}
\examples{
# load example data
library(fergm)
data("ergm.fit")
data("fergm.fit")

# Use built in compare_predictions function to compare predictions of ERGM and FERGM,
# few replications due to example
# Make sure "net" is an object defined in the global environment.
net <- ergm.fit$network
predict_out <- compare_predictions(ergm.fit = ergm.fit, fergm.fit = fergm.fit,
                                   replications = 10, seed=12345)

# Use the built in compare_predictions_plot function to examine the densities of
#  correctly predicted ties from the compare_predictions simulations
compare_predictions_plot(predict_out)

# We can also conduct a KS test to determine if the FERGM fit
     # it statistically disginguishable from the ERGM fit
compare_predictions_test(predict_out)
}
\references{
Box-Steffensmeier, Janet M., Dino P. Christenson, and Jason W. Morgan. 2018. ``Modeling Unobserved Heterogeneity in Social Networks with the Frailty Exponential Random Graph Model." \emph{Political Analysis}. (26)1:3-19.

Stan Development Team (2016). RStan: the R interface to Stan. R package version 2.14.1. \url{http://mc-stan.org/}.
}
\keyword{Fit}
\keyword{GOF}
\keyword{Prediction.}
