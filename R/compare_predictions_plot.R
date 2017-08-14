#' Compare FERGM to ERGM percentages correctly predicted via plot
#' This is a plot function to compare the distributions of predictions from ERGM and FERGM objects.  Uses ggplot2.
#' @param compare_predictions_out Matrix of correctly predicted ties produced by the compare_predictions function.
#' @keywords Fit GOF Prediction Plot
#' @return The compare_predictions_plot function returns a ggplot of the density of the percent of correctly predicted ties simulated by the compare_predictions function
#' @references Box-Steffensmeier, Janet M., Dino P. Christenson, and Jason W. Morgan. 2017. ``Modeling Unobserved Heterogeneity in Social Networks with the Frailty Exponential Random Graph Model." \emph{Political Analysis}.
#' @references Stan Development Team (2016). RStan: the R interface to Stan. R package version 2.14.1. \url{http://mc-stan.org/}.
#' @example
#' # load example data
#' data("ergm.fit")
#' data("fergm.fit")
#' data("mesa")
#' # Use built in compare_predictions function to compare predictions of ERGM and FERGM,
#' # few replications due to example.
#' predict_out <- compare_predictions(ergm_fit = ergm.fit, fergm_fit = fergm.fit,
#'                                    replications = 10)
#' # Use the built in compare_predictions_plot function to examine the densities
#' # of correctly predicted ties from the compare_predictions simulations
#' compare_predictions_plot(predict_out)
#'
#' @export

compare_predictions_plot <- function(compare_predictions_out = NULL){
  plot_df <- reshape2::melt(as.data.frame(compare_predictions_out))
  p <- ggplot(data = plot_df, aes(x = plot_df$value, color = plot_df$variable, fill = plot_df$variable)) +
    geom_density(alpha = 0.5) +
    xlab("Percent of Ties Correctly Predicted") +
    ylab("Density") +
    scale_fill_manual(values=c("firebrick4", "dodgerblue4"),
                      name="Model",
                      breaks=c("pct_correct_ergm", "pct_correct_fergm"),
                      labels=c("ERGM", "FERGM")) +
    scale_color_manual(values=c("firebrick4", "dodgerblue4"),
                      name="Model",
                      breaks=c("pct_correct_ergm", "pct_correct_fergm"),
                      labels=c("ERGM", "FERGM")) +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))

  return(p)
}
