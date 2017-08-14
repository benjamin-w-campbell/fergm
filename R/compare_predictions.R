#' Compare FERGM to ERGM to assess accuracy in tie prediction
#'
#' This function allows you to assess the importance of the frailty term in prediction
#' @param ergm_fit An ERGM object that will be compared to the FERGM fit.
#' @param fergm_fit The output of the fergm function that will be compared to the ERGM fit.
#' @param seed An integer that sets the seed for the random number generator to assist in replication.  Defaults to 12345.
#' @param replications The number of networks to be simulated to assess predictions. Defaults to 500.
#' @keywords Fit GOF Prediction.
#' @references Box-Steffensmeier, Janet M., Dino P. Christenson, and Jason W. Morgan. 2017. ``Modeling Unobserved Heterogeneity in Social Networks with the Frailty Exponential Random Graph Model." \emph{Political Analysis}.
#' @references Stan Development Team (2016). RStan: the R interface to Stan. R package version 2.14.1. \url{http://mc-stan.org/}.
#' @return The compare_predictions function returns a matrix reflecting the number of correctly predicted ties for the ERGM and FERGM for each network simulated.
#' @examples
#' # load example data
#' data("ergm.fit")
#' data("fergm.fit")
#' data("mesa")
#'
#' # Use built in compare_predictions function to compare predictions of ERGM and FERGM,
#' # few replications due to example
#' predict_out <- compare_predictions(ergm_fit = ergm.fit, fergm_fit = fergm.fit,
#'                                    replications = 10)
#'
#' # Use the built in compare_predictions_plot function to examine the densities of
#' #  correctly predicted ties from the compare_predictions simulations
#' compare_predictions_plot(predict_out)
#'
#' # We can also conduct a KS test to determine if the FERGM fit
#'      # it statistically disginguishable from the ERGM fit
#' compare_predictions_test(predict_out)
#' @export

compare_predictions <- function(ergm_fit = NULL, fergm_fit = NULL, seed = 12345, replications = 500){
  lt <- function(m) { m[lower.tri(m)] }

  ergm.pred <- function()
  {
    flo.truth <- lt(as.matrix(ergm_fit$network))
    sim.pred <- lt(as.matrix(simulate.ergm(ergm_fit)))
    sum(flo.truth == sim.pred) / 630
  }

  pct_correct_ergm <- replicate(replications, ergm.pred())

  stan.dta <- fergm_fit$stan.dta
  stan.fit <- fergm_fit$stan.fit

  truth <- stan.dta$y
  predictions <- extract(stan.fit, "predictions")$predictions

  pct_correct_fergm <- sapply(1:nrow(predictions),
                              function(r) sum(truth == predictions[r,]) / 630)


  correct_mat <- cbind(pct_correct_ergm, pct_correct_fergm)

  improvement <- round(((mean(pct_correct_fergm)-mean(pct_correct_ergm))/mean(pct_correct_ergm))*100, 2)

  cat(paste0("The FERGM fit reflects a ", improvement, "% improvement in tie prediction relative to the ERGM across ", replications, " simulations"))

  return(correct_mat)
}

