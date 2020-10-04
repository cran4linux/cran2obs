#' cleanList takes a package name and uses tools::package_dependencies
#' to construct either a list of OpenSUSE dependencies, which in CRAN
#' means the union of 'Depends', 'Imports' and 'LinkingTo', or
#' OpenSUSE recommends, which are represented in CRAN 'Suggests:'
#' Dependencies are constructed recursively, suggests not!
#' These lists are cleaned from installed base and recommended packages
#' before returned.
#'
#' @param pkgname name of CRAN package
#' @param kind of list: "depends" or "suggests"
#' @param repo repo to build list from
#' 
#' @return Vector of CRAN package names
#' @export
cleanList <- function(pkgname, kind, repo=getOption("c2o.cran")){
    ap = available.packages(repos=repo)
    
    if (kind == "depends") {
        kind <- c("Depends", "Imports", "LinkingTo")
        recur <- TRUE
    } else if (kind == "suggests") {
        kind <- "Suggests"
        recur <- FALSE
    } else {
        stop("cleanList: Unknown type of list requested!")
    }
        
    dep <- tools::package_dependencies(pkgname, db=ap,
                                       which = kind,
                                       recursive=recur, reverse=FALSE )

    rmpkgs <- installed.packages()
    rmpkgs <- rmpkgs[ which( (rmpkgs[, "Priority"] == "base") |
                             (rmpkgs[, "Priority"] == "recommended")), "Package"]
    ## base and recommended always already installed and can be removed from list
    
    depn <- lapply( dep, function(x) x <- x[ ! x %in% rmpkgs  ]  )
    ## remove rmpkgs from dependency list
    
    paste0( unlist(depn), collapse=" ")
}

#' cleanDeps returns a structure like available.package()
#' but including recursive dependencies (Depends, Imports, LinkingTo).
#' Because recommended packages may be assumed to be installed
#' in a OpenSUSE installation with R, these are removed together with
#' base packages from the dependencies.
#'
#' @param repo to build the dependency matrix off
#' 
#' @return A dataframe containing from available.packages() columns "Package",
#' "Version", "License", "NeedsCompilation", a cleaned-up version of "Suggests",
#' and new columns 'recDep', containing all recursive dependencies, and 'depLen'
#' containing the number of recursive dependencies.
#' 
#' @export
cleanDeps <- function(repo=getOption("c2o.cran")){
    ap <- available.packages(repos=repo)

    dep <- tools::package_dependencies(ap[,"Package"], db =ap,
                                       which = c("Depends", "Imports", "LinkingTo"),
                                       recursive=TRUE, reverse=FALSE )
    suggests <- tools::package_dependencies(ap[,"Package"], db =ap,
                                       which = c("Suggests"),
                                       recursive=FALSE, reverse=FALSE )
    ## There are "Suggests" in ap, but package_dependency does very useful normalisation

    ## This takes a few seconds to a minute on a fairly fast computer

    rmpkgs <- installed.packages()
    rmpkgs <- rmpkgs[ which( (rmpkgs[, "Priority"] == "base") |
                             (rmpkgs[, "Priority"] == "recommended")), "Package"]
    ## base and recommended always already installed

    ap <- ap[, c("Package", "Version", "License", "NeedsCompilation")]
    
    depn <- lapply( dep, function(x) x <- x[ ! x %in% rmpkgs  ]  )
    suggestsn <- lapply( suggests, function(x) x <- x[ ! x %in% rmpkgs  ]  )
    ## remove rmpkgs from dependency and suggests list
    
    ap <- data.frame(ap, recDep=as.vector(unlist(lapply(depn, paste0, collapse=" "))))
    ## new column with recursive dependencies
    ap <- data.frame(ap, Suggests=as.vector(unlist(lapply(suggestsn, paste0, collapse=" "))))
    ## new column with suggests

    ap <- cbind(ap, depLen= sapply( ap[,"recDep"], function(x) length( unlist( strsplit( x, " ")))))
    ## new column with number of dependencies
}

#' This function generates an analogous matrix to available.packages()
#' but for all the packages in OBS
#' @param obsproject Project in OBS where packages are taken from.
#' @param quiet If set to FALSE some progression info is given, default = TRUE.
#'
#' @return Matrix of all R packages avaialble in obsproject.
#'
#' @export
available.packages.OBS <- function(obsproject=getOption("c2o.auto")){
    ## the names first
    cmd <- paste("osc ls", obsproject, sep=" ", collapse="")
    obspkgs <- system(cmd, intern=TRUE)

    ## may contain additional stuff related to compiling some packages
    ## only R-* packages are relevant
    if (length(obspkgs) > 0) { ## i.e. not a new empty repo
        obspkgs <- obspkgs[grep("R-", obspkgs)]
        
        ## all packages start with "R-" by convention, see page in build service
        if (obsproject == "devel:languages:R:released") {
            ## other OBS Project shouldn't contain non-CRAN R packages
            obspkgs <- obspkgs[-which(obspkgs=="R-base")]
            ## R-base not an R package
            obspkgs <- obspkgs[-which(obspkgs == "R-base-java")]
            ## R-base-java is not a CRAN package.
        }
        cranpkgnames <- gsub("R-", "", obspkgs)
        obsinfo <- sapply(cranpkgnames, getOBSVersion, obsproject=obsproject)
        obspkgs <- data.frame(Package=as.character(cranpkgnames), OBSVersion=as.character(unlist(obsinfo[1,])), hasDevel=unlist(obsinfo[2,]) )
    } else {
        obspkgs <- data.frame(Package=character(), OBSVersion=character(), hasDevel=character())
    }
    return (obspkgs)
}

#' getOBSVersion takes the name of an R package (from CRAN) and receives the corresponding
#' information off of OBS
#' This function is not exactly cheap run-time wise. Around 0.5s per package.
#' @param pkg name of an R package as found in CRAN
#' @param obsproject Project in OBS where packages are taken from.
#' 
#' @return A list with components file containing the version number and
#' a boolean value hasDevel
#'
#' @export
getOBSVersion2 <- function ( pkg, obsproject=getOption("c2o.auto")) {
#    logger("** get Info off of OBS")
    cmd <- paste0( "osc ls -b ", obsproject, " R-", pkg, " openSUSE_Tumbleweed x86_64", sep="", collapse="")
    lst <- system( cmd , intern=TRUE )
    
    if ( any( grep( "-devel", lst))) hasDevel <- TRUE else hasDevel=FALSE 

    src <- lst[ grep( "src.rpm", lst) ]
    srcversion <- unlist( strsplit( gsub( paste0( "R-", pkg, "-"), "", src), "-"))[1]
    
    return(list(version=srcversion, hasDevel=hasDevel))
}


#' getOBSVersion takes the name of an R package (from CRAN) and receives the corresponding
#' information off of OBS
#' This function is not exactly cheap run-time wise. Around 0.5s per package.
#' @param pkg name of an R package as found in CRAN
#' @param obsproject Project in OBS where packages are taken from.
#' @param quiet If set to FALSE some progression info is given, default = TRUE.
#' 
#' @return A list with components file containing the source file name and
#' version containing the version.
#'
#' @export
## getOBSVersion <- function ( pkg, obsproject=getOption("c2o.auto"), quiet=TRUE ) {
##     if (! quiet) cat( "Checking ", pkg, "\n")
##     cmd <- paste( "osc ls ", obsproject, " R-", pkg, sep="", collapse="")
##     lst <- system( cmd , intern=TRUE )
##     srcfile <- lst[ grep( paste( "^", pkg, sep="", collapse=""), lst)][1]
##     ## the last [1] is needed if a package is linked to factory. In lst the files
##     ## can appear multiple times then.
##     srcversion <- gsub(paste0( pkg, "_"), "", gsub(".tar.gz", "", srcfile))
##     return(srcversion)
## }

#' CranOBSfromScratch combines available.packages() and info of two OBS repos, i.e.
#' home:detlef:AutomaticCRAN and d:l:R:released into a datafram combining all information
#' about the build status of the different packages.
#' @param repo1 and 
#' @param repo2 Repos containing automatic builds and manually improved builds resp.
#'
#' @return A dataframe like cleanDeps with additional infos for OBS packages
#' 
#' @export
CranOBSfromScratch <- function(cran=getOption("c2o.cran") , repo1=getOption("c2o.auto"), repo2=getOption("c2o.manual")){
    cran <- cleanDeps(cran)
    automatic <- available.packages.OBS(obsproject=repo1)
    released  <- available.packages.OBS(obsproject=repo2)
    obs <- merge(automatic, released[,c("Package", "OBSVersion")], all=TRUE, suffixes=c(".a", ".r"), by="Package")
    status <- merge( cran, obs, by="Package" , all=TRUE )
    status <- cbind(status, triedVersion=NA)
    status$Row.names <- NULL
    for (col in 1:dim(status)[2]) status[,col] <- as.character(status[,col])  
    return(status)
}
