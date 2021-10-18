#' Calculate the highest density interval
#'
#' Calculate the highest density interval (HDI).
#'
#' The HDI is the shortest interval with the required density.
#' Note that in general, HDI can be split into several subintervals if te underlying distribution
#' is multimodal. This function however provide only continuous HDI.
#'
#' Also note that the HDI intervals, unlike the more common equal-tailed intervals,
#' are not invariant under non-linear transformations.
#'
#' @param x an estimated density, such as the one returned by the `[stats::density()]` function,
#' with x and y values describing the shape of the density
#' @param size the size of both tails or `1 - alpha`
#' @param alpha the size of both tails or `1 - interval`
#' @return upper and lower bounds for a selected HDI interval
#'
#' @examples
#' x = rnorm(100)
#' dens = density(x)
#' hdi(dens, alpha=0.05) # the 95% HDI
#' hdi(dens, alpha=0.5) # the 50% HDI
#'
#' @export
hdi = function(x, size=0.95, alpha=1-size){
    # assume that density is unimodal
    cumul = cumsum(x$y) / sum(x$y)
    lower_index = upper_index = which(cumul < alpha)
    for(i in lower_index)
        upper_index[i] = min(which(cumul > cumul[i] + 1 - alpha))
    widths = upper_index - lower_index
    best = which.min(widths)
    result = c(
        lower = mean(x$x[lower_index[best]]),
        upper = mean(x$x[upper_index[best]])
        )
    result
    }
