#' Calculate the highest density interval
#'
#' Calculate an empirical highest density interval (HDI).
#'
#' The HDI is the shortest interval that contains the required proportion of values.
#' In general, HDI can be split into several subintervals if the underlying distribution
#' is multimodal. This function however provide only continuous HDI and is correct only for unimodal
#' distributions.
#'
#' The HDI intervals, unlike the more common equal-tailed intervals,
#' are not invariant under non-linear transformations.
#'
#' @param x a vector of values or an estimated density, such as the one returned by the `[stats::density()]` function,
#' with x and y values describing the shape of the density
#' @param size the size of both tails or `1 - alpha`
#' @param alpha the size of both tails or `1 - interval`
#' @return upper and lower bounds for a selected HDI interval
#'
#' @examples
#' x = rnorm(100)
#' hdi(dens, alpha=0.05) # the 95% HDI
#'
#' # Alternatively, use kernel-density estimation to estimate the empirical density distribution
#' dens = density(x)
#' hdi(dens, alpha=0.05) # the 95% HDI
#' hdi(dens, alpha=0.5) # the 50% HDI
#'
#' @export
hdi = function(x, size=0.95, alpha=1-size){
    UseMethod("hdi", x)
    }


#' rdname hdi
#' @export
hdi.density = function(x, size=0.95, alpha=1-size){
    # assume that density is unimodal
    cumul = cumsum(x$y) / sum(x$y)
    lower_index = upper_index = which(cumul < alpha)
    for(i in lower_index)
        upper_index[i] = min(which(cumul > cumul[i] + 1 - alpha))
    widths = upper_index - lower_index
    best = which.min(widths)
    
    # If there are multiple best and are not sequential, pick the leftmost one
    # otherwise, the value is averaged
    if(length(best) > 1 && !is_sequential(best))
        best = best[1]
    
    result = c(
        lower = mean(x$x[lower_index[best]]),
        upper = mean(x$x[upper_index[best]])
        )
    result
    }


#' rdname hdi
#' @export
hdi.default = function(x, size=0.95, alpha=1-size){
    n = length(x)
    x = sort.int(x)
    
    window_size = ceiling((1-alpha) * n)
    
    if(window_size < 2)
        stop("Size of the highest density interval is too small or",
             " the input vector does not contain enough data points.")
    if(n - window_size < 1)
        stop("Size of the highest density interval is too large or",
             " the input vector does not contain enough data points.")

    
    lower_index = seq_len(n - window_size)
    upper_index = window_size + lower_index
    best = which.min(x[upper_index] - x[lower_index])
    
    # if there are multiple best and are sequential, pick the central one
    # otherwise, pick the first one
    if(length(best) > 1 && is_sequential(best)){
        best = floor(mean(best))
        } else {
        best = best[1]
        }
    
    result = c(
        lower = x[lower_index[best]],
        upper = x[upper_index[best]]
        )
    result
    }


#' Test if values are sequential
#'
#' Values are sequential only if differences between each value is always exactly one.
#'
#' @param x a vector of values
#' @return a TRUE/FALSE value
is_sequential = function(x){
    all(diff(x) == 1)
    }
