#' Create a random string
#'
#' Create a random string from letters and numbers
#'
#' @param x a length of the random string
#' @param letters a number of letters
#' @param numbers a number of numerals
#' @return a random string of length `x` from letters and numbers
random_string = function(x, letters=NULL, numbers=NULL){
    if(is.null(letters) && is.null(numbers))
        letters = sample(x, 1)
    if(is.null(letters))
        letters = x - numbers
    if(is.null(numbers))
        numbers = x - letters
    if( (letters + numbers) > x)
        stop("More letters and numbers than the length of the word")

    letters = sample(base::letters, letters, replace=TRUE)
    numbers = sample(1:9, numbers, replace=TRUE)
    paste0(sample(c(letters, numbers)), collapse="")
    }
