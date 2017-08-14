#' Plot traceplots for model terms.
#'
#' This function takes a fergm object and plots the time series of each chain per model term.
#' @param fergm.fit Output object from the fergm function.
#' @param custom_var_names If custom variable names are to be used, specify a vector of characters equal to the number of terms used.
#' @param form The right hand side formula specified for the FERGM function, custom.var.names takes precedent.
#' @return This prints a traceplot for the effects of interest
#' @keywords FERGM interpret summary
#' @references Box-Steffensmeier, Janet M., Dino P. Christenson, and Jason W. Morgan. 2017. ``Modeling Unobserved Heterogeneity in Social Networks with the Frailty Exponential Random Graph Model." \emph{Political Analysis}.
#' @references Stan Development Team (2016). RStan: the R interface to Stan. R package version 2.14.1. \url{http://mc-stan.org/}.
#' @examples
#' # load example data
#' data("ergm.fit")
#' data("fergm.fit")
#' data("mesa")
#' # Use rstan's built in traceplot function
#' trace <- rstan::traceplot(fergm.fit$stan.fit, pars = "beta")
#' trace
#'
#' # We have our own version that includes variable names and tidies it up a bit
#' fergm_beta_traceplot(fergm.fit,
#'                     form = NULL,
#'                     custom_var_names =  c("Edges", "Sex Homophily",
#'                     "Grade Homophily", "Race Homophily", "GWESP", "Alternating K-Stars"))
#
#' @export

fergm_beta_traceplot <- function(fergm.fit = NULL, custom_var_names = NULL, form = NULL){
  trace <- rstan::traceplot(fergm.fit$stan.fit, pars = "beta")

  if(is.null(custom_var_names)){
    var_names <- unlist(strsplit(form, "[+]"))
    var_names<- stringr::str_replace_all(string=var_names, pattern=" ", repl="")
    levels(trace$data$parameter) <- var_names
    trace$labels$colour <- "Chain"
    trace$labels$x <- "Iteration"
    trace$labels$y <- "Value"
  } else {
    var_names <- custom_var_names
    levels(trace$data$parameter) <- var_names
    trace$labels$colour <- "Chain"
    trace$labels$x <- "Iteration"
    trace$labels$y <- "Value"
  }
  return(trace)
}
