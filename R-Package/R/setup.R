#' .onLoad prints the version number and checks settings of the package configuration
#' A warning is given, if settings are not complete!
#' @return No return value

.onLoad <- function(libname, pkgname){
    packageStartupMessage("This is CRAN2OBS version ", utils::packageDescription("CRAN2OBS", field="Version"), appendLF=TRUE)
    packageStartupMessage("CRAN2OBS needs a few options set to function properly", appendLF=TRUE)
    failed <- FALSE
    
    if (! is.null(getOption("c2o.cran") )) {
        cat("Option c2o.cran is set to ", getOption("c2o.cran") , "\n")
    } else {
        cat("Set option 'c2o.cran' to something, where available.packages() works. \n")
        cat("I.e. getOption(c2o.cran = \"https://cloud.r-project.org\") for a main CRAN mirror.\n")
        failed <- TRUE
    }

    if (! is.null(getOption("c2o.auto") )) {
        cat("Option 'c2o.auto' is set to ", getOption("c2o.auto") , "\n")
    } else {
        cat("Set option 'c2o.auto' to the remote project name in OBS, where automatically maintained packages reside\n")
        cat("I.e. getOption(c2o.auto = \"home:dsteuer:AutomaticCRAN\") for my test repo.\n")
        failed <- TRUE
    }
    
    if (! is.null(getOption("c2o.manual") )) {
        cat("Option 'c2o.manual' is set to ", getOption("c2o.manual") , "\n")
    } else {
        cat("Set option 'c2o.manual' to the remote project name in OBS, where additional, manually maintained packages reside\n")
        cat("I.e. getOption(c2o.manual = \"devel:languages:R:released\") for the main OBS R repo.\n")
        failed <- TRUE
    }

    if (! is.null(getOption("c2o.localOBS") )) {
        cat("Option 'c2o.localOBS' is set to ", getOption("c2o.localOBS") , "\n")
    } else {
        cat("Set option 'c2o.localOBS' to the local directory, where packages will be created for or checked out from OBS\n")
        cat("I.e. getOption(c2o.localOBS = \"~/OBS\").\n")
        failed <- TRUE
    }

    if (! is.null(getOption("c2o.download.cache") )) {
        cat("Option 'c2o.download.cache' is set to ", getOption("c2o.download.cache") , "\n")
    } else {
        cat("Set option 'c2o.download.cache' to the directory, where rpmbuild expects the sources.\n")
        cat("I.e. getOption(c2o.download.cache = \"/home/yourusername/rpmbuild/SOURCES\"), if you did not alter the rpmbuild config from its default.\n")
        failed <- TRUE
    }

    if (! is.null(getOption("c2o.binary.cache") )) {
        cat("Option 'c2o.binary.cache' is set to ", getOption("c2o.binary.cache") , "\n")
    } else {
        cat("Set option 'c2o.binary.cache' to the local directory, where rpm packages will be created by rpmbuild\n")
        cat("I.e. getOption(c2o.binary.cache = \"/home/yourusername/rpmbuild/RPMS/x86_64\"), if you did not alter the rpmbuild config from its default.\n")
        failed <- TRUE
    }

    if (! is.null(getOption("c2o.statusfile") )) {
        cat("Option 'c2o.statusfile' is set to ", getOption("c2o.statusfile") , "\n")
        options(c2o.status = read.table(getOption("c2o.statusfile", header=TRUE, sep=";")))
        ## TODO check if it works!
    } else {
        cat("Set option 'c2o.statusfile' to the filename, where the status of the current sync effort are stored.\n")
        cat("I.e. getOption(c2o.statusfile = \"CRANinOBSStatus.csv\").\n")
        cat("Will be (re-)created if neccassary, what will take a few minutes\n")
        failed <- TRUE
    }

    if (! is.null(getOption("c2o.logfile") )) {
        cat("Option 'c2o.logfile' is set to ", getOption("c2o.logfile") , "\n")
    } else {
        cat("Set option 'c2o.logfile' to the filename, where the log of the current sync effort shall be stored.\n")
        cat("I.e. getOption(c2o.logfile = \"buildlog.org\").\n")
        failed <- TRUE
    }
    
    if (failed) warning("Not all options to build rpms from CRAN are set! Use CRAN2OBS::check.settings().")
}

#' check.settings validates all options settings
#'
#' @return TRUE if build can start, FALSE otherwise, but tries to give hints.
#'
#' @export

check.settings <- function(){
    ## TODO must fill out
}
