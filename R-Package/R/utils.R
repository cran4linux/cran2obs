#' logger writes a character string in the console and append it to file
#' If msg is more than one line, only written to file
#' @param msg character string
#' @param file to write to
#' @return no return value
logger <- function( msg, file=getOption("c2o.logfile") {
    if (length(msg == 1))  cat( msg, "\n")
    for ( line in msg) {
        cat( line, "\n", file=file, append=TRUE)
    }
}
