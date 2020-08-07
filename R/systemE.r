#' Run an external command
#'
#' A wrapper around the system2 function that raise an error if the external
#' external command resulted in an error. See the \code{link{system2}} function
#' for more information.
#'
#' @param command an external command to run
#' @param args an arguments to the command
#' @param call prints the command call, useful for debugging
#' @param dir run the command in a target directory. Some external commands create output
#'     in the directory they are executed. This is often less than ideal and leads to an unwanted
#'     shuffling with files or increased complexity to accomodate to this behaviour.
systemE = function(command, args, call=FALSE, dir=NA){
    if(call) message("Call: ", command, " ", paste0(args, collapse=" "))

    if(!is.na(dir)){
        on.exit(setwd(wd)) # an additional safety precaution if a normal run is aborted
        wd = getwd()
        setwd(dir)
        }

    status = system2(command, args)

    if(!is.na(dir))
        setwd(wd)

    if(status != 0)
        stop("external command \"", command, "\" exit status: ", status)
    }
