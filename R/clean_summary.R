#' Clean summary of stan output
#'
#' This function allows you to to take a stan object and return a clean summary of the posterior distribution
#' @param fergm.fit The returned output object of the fergm() function.  Null by default
#' @param form The right hand side formula specified for the FERGM function.
#' @param custom_var_names A vector of custom variable names in the order of the form object.
#' @return This prints a matrix summarizing the posterior distribution
#' @keywords FERGM interpret summary
#' @references Box-Steffensmeier, Janet M., Dino P. Christenson, and Jason W. Morgan. 2017. ``Modeling Unobserved Heterogeneity in Social Networks with the Frailty Exponential Random Graph Model." \emph{Political Analysis}.
#' @references Stan Development Team (2016). RStan: the R interface to Stan. R package version 2.14.1. \url{http://mc-stan.org/}.
#' @examples
#' # The fergm.fit$stan.fit object is of class stanfit.
#'    # We keep it this way such that users can rely upon
#'    # conventional stan functions for interpretation
#'    # getting posterior distributions from the fergm
#'
#'    # Conventional rstan summary call
#'
#'  # load example data
#' data("ergm.fit")
#' data("fergm.fit")
#' data("mesa")
#' stan.smry <- summary(fergm.fit$stan.fit)$summary
#' beta_df <- stan.smry[grep("beta", rownames(stan.smry)),]
#' est <- round(beta_df[,c(1,4,8)], 3)
#' est # in order of "form"
#' form = c("edges + nodematch('Sex') + nodematch('Grade', diff = FALSE) +
#'          nodematch('Race', diff = FALSE) +  gwesp(decay = 0.2, fixed = TRUE) +
#'          altkstar(lambda = 0.6, fixed = TRUE)")
#'
#'   # We have a built in function to do this simply
#' est <- clean_summary(fergm.fit, form = form)
#' est <- clean_summary(fergm.fit,
#' custom_var_names = c("Edges", "Sex Homophily", "Grade Homophily",
#' "Race Homophily", "GWESP", "Alternating K-Stars"))
#' @export

clean_summary <- function(fergm.fit = NULL, form = NULL, custom_var_names = NULL){
  its <- rstan::extract(fergm.fit$stan.fit)$beta

  fergm_df <- cbind(as.data.frame(colMeans(its)), as.data.frame(matrixStats::colQuantiles(its, probs = c(0.025, 0.975))))
  colnames(fergm_df)[1] <- "mean"

  if(!is.null(custom_var_names)){
    rownames(fergm_df) <- custom_var_names
  } else {
    rownames(fergm_df) <- stringr::str_replace_all(string = unlist(strsplit(form, "[+]")), pattern=" ", repl="")
  }

  print(fergm_df)
  return(fergm_df)
}
