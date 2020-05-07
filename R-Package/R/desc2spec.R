#' buildinstallrpm takes a R package name and tries to
#' build and locally install the corresponding rpm package
#'
#' #' @param packname A package name of a package in a R repo like CRAN
#' @param rpmbuildroot The directory where rpmbuild should do its work
#' 
#' @return No value
#' @export
buildinstallrpm <- function(packname, rpmbuildroot="~/rpmbuild/" ){
    log <- "bi.err"
    cat("Begin building", packname, "\n")
    cat("Begin building", packname, "\n", file=log, append=TRUE)
    cat(as.character( Sys.time()), "\n", file=log, append=TRUE)
    result <- desc2spec(packname, rpmbuildroot=rpmbuildroot)
    if (grepl(".spec", result, fixed=TRUE)) { #success, now install built rpm
        cat("Successfully built ", packname, " rpm\n", file=log, append=TRUE)
        rpmname <- paste(rpmbuildroot,"RPMS/x86_64/R-", packname,"*.rpm", sep="")
        ## Installation only works, if R runs as root. You habe been warned
        suppressWarnings(
            result <- system2("rpm", args=c("--install --force", rpmname),stdout=TRUE, stderr=TRUE)
        )
        for (line in result) cat(line, "\n", file=log, append=TRUE)
    } else {
        cat("Failed to build and install ", packname, "\n", result, "\n")
        cat("Failed to build and install ", packname, "\n", result, "\n", file=log, append=TRUE)
    }
    cat("Finished ",packname,"\n\n")
    cat("Finished ",packname,"\n", file=log, append=TRUE)
    cat(as.character(Sys.time()), "\n", file=log, append=TRUE)
}

#' desc2spec takes the name of an R package and generates a spec file
#' to be used in OBS. The external tool rpmbuild is used to build
#' the OpenSUSE package. The resulting spec file will reside where
#' rpmbuild puts it. The %check section is deliberately empty. We rely
#' on the checks on CRAN.
#'
#' @param packname A package name of a package in a R repo like CRAN
#' @param rpmbuildroot The directory where rpmbuild should do its work
#' 
#' @return The path to the generated specfile.
#' @export
desc2spec <- function(packname, rpmbuildroot="~/rpmbuild/") {
    errorlog <- "desc2spec.err"
    cat("Building ", packname, "\n", file=errorlog, append=TRUE)
    cat("Begin ", as.character(Sys.time()), "\n", file=errorlog, append=TRUE)

    if (! file.exists(system.file("specfile.tpl", package="CRAN2OBS"))){
        stop("Specfile template not found. Exit.")
    }
    spectpl <- readLines(system.file("specfile.tpl", package="CRAN2OBS"))
    
    ap <- available.packages(repos="https://cloud.r-project.org")

    if (! packname %in% ap[,"Package"]) {
        cat("Seems ", packname, " no longer exists on CRAN\n")
        cat("Seems ", packname, " no longer exists on CRAN\n", file=errorlog, append=TRUE)
        return(paste("Failed: Package", packname, " no longer on CRAN"))
    }
    
    version <- ap[Package=packname,"Version"]
    
    
    source0 <-  paste(packname,"_",version,".tar.gz",sep="")
    version <- gsub("-",".",ap[Package=packname,"Version"])
    ## version must be normalized for rpm
    
    if (! file.exists( paste(rpmbuildroot,"SOURCES/",source0, sep=""))) {
        download.file(paste("https://cloud.r-project.org/src/contrib/", source0, sep=""),
                      paste(rpmbuildroot,"SOURCES/",source0, sep=""))
    }
    
### extract DESCRIPTION file, read it, erase it
    untar( paste(rpmbuildroot,"SOURCES/",source0, sep=""), paste(packname,"/DESCRIPTION",sep="") )
    
    description <- readLines(paste(packname,"/DESCRIPTION",sep=""))
    unlink(packname,recursive=TRUE)
    ### unclutter!
    
### template and descrition have been read in
### now populate the specfile tempalte
    
    spectpl <- gsub( "{{year}}", format( Sys.time(), "%Y" ), spectpl, fixed=TRUE)    
    spectpl <- gsub( "{{packname}}", packname, spectpl, fixed=TRUE)
    spectpl <- gsub( "{{version}}", version, spectpl, fixed=TRUE)	

    summary.str <- sub( "Title: ", "", description[ grep("Title:", description) ], fixed=TRUE)
    spectpl <- gsub( "{{summary}}", summary.str, spectpl, fixed=TRUE)

    license <- sub( "License: ", "", description[ grep("License:", description) ], fixed=TRUE)
    spectpl <- gsub( "{{license}}", license, spectpl, fixed=TRUE)

    spectpl <- gsub( "{{source0}}", source0, spectpl, fixed=TRUE)
    deps <-  cleanList( packname, "depends" )
    if (length(deps) >0) {
        deps <- paste( "R-", deps, sep="")
    } else {
        deps <- c()
    }
    suggests <- cleanList( packname, "suggests")
    if (length(suggests) > 0) {
        suggests <- paste( "R-", suggests, sep="")
    } else {
        suggests <- c()
    }

    descstart <- grep("Description:", description, fixed=TRUE)
    descend <- descstart + grep( "^[A-Z][a-z]+:", description[descstart+1:length(description)])[1] -1
    description.str <- trimws(paste( description[ descstart:descend ] ), which="both")
    description.str[1] <- gsub("Description: ", "", description.str[1])
    ## still needs formating! textwrap for R?
    
    needs.compilation <- ap[Package=packname, "NeedsCompilation"]

    specfile <- paste(rpmbuildroot,"SPECS/R-",packname,".spec",sep="")
#    cat("specfile goes here ",specfile, "\n")
    if (file.exists(specfile)) unlink(specfile)  ## We generate a new file! This is not update!
    cat("# Automatically generated by CRAN2OBS::desc2spec\n", file=specfile)
#    for (line in spectpl) {
#        cat( line, "\n")
#    }
    for (line in spectpl) {
        if ( grepl( "{{depends}}", line, fixed=TRUE)) {
            if (length(deps) > 0) {
                for (item in deps) cat( "Requires:\t", item, "\n", sep="", file=specfile, append=TRUE)
            } else { next }
        } else if ( grepl( "{{builddepends}}", line, fixed=TRUE)) {
            if  (length(deps) > 0)  {
                for (item in deps) cat( "BuildRequires:\t", item, "\n", sep="", file=specfile, append=TRUE)
                } else { next }
        } else if ( grepl( "{{suggests}}", line, fixed=TRUE) ) {
            if (length(suggests) > 0)  {
                for (item in suggests) cat( "Recommends:\t", item, "\n",  sep="", file=specfile, append=TRUE)
            } else {next}
        } else if ( grepl( "{{needscompilation}}", line, fixed=TRUE) ) {
            if (needs.compilation == "yes") cat( "BuildRequires:   gcc gcc-c++ gcc-fortran\n", file=specfile, append=TRUE)
        } else if ( grepl( "{{description}}", line, fixed=TRUE) ) {
            for (item in description.str) cat( item, "\n", file=specfile, append=TRUE)
        } else {
            cat( line, "\n", file=specfile, append=TRUE)
        }
    }

### call rpmbuild
    buildcommand <- paste( "rpmbuild -ba ", specfile ," &> ", specfile, ".log", sep="")

    suppressWarnings(
        rpmlog <- system2("rpmbuild", args=c("-ba", specfile), stdout=TRUE, stderr=TRUE)
    ## the first build will fail by construction.
    ## the output of rpmbuild allows to build the %file section
    ## in the spec
    )

    ## but we have to look for other errors preventing successful second run
    
    if ( length( grep( "Failed build dependencies", rpmlog, fixed=TRUE)) >0 ) { ### missing dependencies, no chance
        cat( "The following missing dependencies must be installed to build R-", packname, "\n", sep="")
        cat( "The following missing dependencies must be installed to build R-", packname, "\n", sep="", file=errorlog, append=TRUE)
        for (line in rpmlog) if ( grepl( "needed", line, fixed=TRUE)) {
                                 cat( line,"\n")
                                 cat( line,"\n", file=errorlog, append=TRUE)
                                 cat("Failed ", as.character(Sys.time()), "\n", file=errorlog, append=TRUE)
                             }
        return("Failed: Missing dependencies first run rpmbuild")
    }

    if (length( grep("Bad exit status", rpmlog, fixed=TRUE)) > 0 ) {
        cat( "Bad exit status from build R-", packname, "\n", sep="")
        cat( "Bad exit status from build R-", packname, "\n", sep="", file=errorlog, append=TRUE)
        return("Failed: Bad exit status")
    }

### At this point something was build. But as the specfile.tpl has an empty file section.
### that part must be constructed from error logs
    
    rpmlog <- trimws( rpmlog[ (grep( "RPM build errors", rpmlog, fixed=TRUE)+2):length(rpmlog)], which="left")
    rpmlog <- gsub(paste("/usr/lib64/R/library/",packname,"/",sep=""), "", rpmlog)

    dirlist <- NULL
    for (line in rpmlog){
        if ( grepl( "/", line, fixed=TRUE) ) { ## a file in a subdirectory, extract the unique directories
            nextdir <- unlist( strsplit( line, "/") )[1]
            if (! nextdir %in% dirlist){
                dirlist <- c( dirlist, nextdir )
                if (grepl( "html", line, fixed=TRUE) | grepl( "help", line, fixed=TRUE)) {
                    cat( "%doc %{rlibdir}/%{packname}/", nextdir, "\n", sep="", file=specfile, append=TRUE)
                } else {
                    cat( "%{rlibdir}/%{packname}/", nextdir, "\n", sep="", file=specfile, append=TRUE)
                }
            }
        } else { ## a file
            if ( grepl( "LICENSE", line, fixed=TRUE)) {
                cat( "%license %{rlibdir}/%{packname}/", line, "\n", sep="", file=specfile, append=TRUE )
            } else if (grepl( "DESCRIPTION", line, fixed=TRUE)) {
                cat( "%doc %{rlibdir}/%{packname}/", line, "\n", sep="", file=specfile, append=TRUE )
            } else if (grepl( "NEWS", line, fixed=TRUE)) {
                cat( "%doc %{rlibdir}/%{packname}/", line, "\n", sep="", file=specfile, append=TRUE )
            } else {
                cat( "%{rlibdir}/%{packname}/", line, "\n", sep="", file=specfile, append=TRUE)
            }
        }
    }

### second build!
    rpmlog <- system2("rpmbuild", args=c("-ba", specfile), stdout=TRUE, stderr=TRUE )

#    rpmlog <- readLines( paste( specfile, ".log", sep=""))
    if (length( grep( "Wrote:", rpmlog, fixed=TRUE)) == 2) {
        cat("Successfully created rpm package\n")
        cat("Successfully created rpm package\n", file=errorlog, append=TRUE)
        print(rpmlog[grep("Wrote:", rpmlog, fixed=TRUE)])
        for (line in  rpmlog[grep("Wrote:", rpmlog, fixed=TRUE)]){
            cat(line, "\n", file=errorlog, append=TRUE)
        }
        cat("Succeded ", as.character(Sys.time()), "\n", file=errorlog, append=TRUE)
    }
    else {
        cat( "Failed to build ", packname, " in second run rpmbuild\n", sep="")
        return("Failed: Unknown error second run rpmbuild")
    }
    return(specfile)
}

#' cleanList takes a package name and uses tools::package_dependencies
#' to construct either a list of OpenSUSE dependencies, which in CRAN
#' means the union of 'Depends', 'Imports' and 'LinkingTo', or
#' OpenSUSE recommends, which are represented in CRAN 'Suggests:'
#' Dependencies are constructed recursively, suggests not!
#' These lists are cleaned from installed base and Recommended packages
#' before returned.
#'
#' @return Vector of CRAN package names
#' @export
cleanList <- function(pkgname, kind){
    ## Option to use predefined CRAN-alike repository should be added.
    if (kind == "depends") {
        kind <- c("Depends", "Imports", "LinkingTo")
        recur <- TRUE
    } else if (kind == "suggests") {
        kind <- "Suggests"
        recur <- FALSE
    } else {
        stop("cleanList: Unknown type of list requested!")
    }
        
    dep <- tools::package_dependencies(pkgname,
                                       which = kind,
                                       recursive=recur, reverse=FALSE )

    rmpkgs <- installed.packages()
    rmpkgs <- rmpkgs[ which( (rmpkgs[, "Priority"] == "base") |
                             (rmpkgs[, "Priority"] == "recommended")), "Package"]
    ## base and recommended always already installed and can be removed from list
    
    depn <- lapply( dep, function(x) x <- x[ ! x %in% rmpkgs  ]  )
    ## remove rmpkgs from dependency list
    
    unlist(depn)
}
