#' desc2spec takes the name of an R package and generates a spec file
#' to be used in OBS. The external tool rpmbuild is used to build
#' the OpenSUSE package. The resulting spec file will reside where
#' rpmbuild puts it. The %check section is deliberately empty. We rely
#' on the checks on CRAN.
#'
#' @return The path to the generated specfile.
#' @export
desc2spec <- function(packname, rpmbuildroot="~/rpmbuild/") {
    spectpl <- readLines(system.file("specfile.tpl", package="CRAN2OBS"))
    
    ap <- available.packages(repos="https://cloud.r-project.org")
    
    version <- ap[Package=packname,"Version"]
    source0 <-  paste(packname,"_",version,".tar.gz",sep="")
    
    if (! file.exists( paste(rpmbuildroot,"SOURCES/",source0, sep=""))) {
        download.file(paste("https://cloud.r-project.org/src/contrib/", source0, sep=""),
                      paste(rpmbuildroot,"SOURCES/",source0, sep=""))
    }
    
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
    deps <- paste( "R-", cleanList( packname, "depends" ), sep="")

    suggests <- paste( "R-", cleanList( packname, "suggests" ), sep="")

    descstart <- grep( "Description:", description, fixed=TRUE)
    descend <- descstart + grep( "^[A-Z][a-z]+:", description[descstart+1:length(description)])[1] -1
    description.str <- paste( description[ descstart:descend ] )
    description.str[1] <- gsub("Description: ", "", description.str[1])
    ## still needs formating! textwrap for R?
    
    needs.compilation <- sub("NeedsCompilation: ", "", description[ grep("NeedsCompilation:", description) ],
                             fixed=TRUE)

    specfile <- paste(rpmbuildroot,"SPECS/R-",packname,".spec",sep="")
    if (file.exists(specfile)) unlink(specfile)  ## We generate a new file! This is not update!
    
    for (line in spectpl) {
        if ( grepl( "{{depends}}", line, fixed=TRUE) ) {
            for (item in deps) cat( "Requires:\t", item, "\n", sep="", file=specfile, append=TRUE)
        } else if ( grepl( "{{builddepends}}", line, fixed=TRUE) ) {
            for (item in deps) cat( "BuildRequires:\t", item, "\n", sep="", file=specfile, append=TRUE)
        } else if ( grepl( "{{suggests}}", line, fixed=TRUE) ) {
            for (item in suggests) cat( "Recommends:\t", item, "\n",  sep="", file=specfile, append=TRUE)
        } else if ( grepl( "{{needscompilation}}", line, fixed=TRUE) ) {
            if (needs.compilation == "yes") cat( "BuildRequires:\tgcc gcc-c++ gcc-fortran\n", file=specfile,
                                                append=TRUE)
        } else if ( grepl( "{{description}}", line, fixed=TRUE) ) {
            for (item in description.str) cat( item, "\n", file=specfile, append=TRUE)
        } else {
            cat( line, "\n", file=specfile, append=TRUE)
        }
    }

### call rpmbuild
    buildcommand <- paste( "rpmbuild -ba ", specfile ," &> ", specfile, ".log", sep="")
    system(buildcommand )

    rpmlog <- readLines( paste( specfile, ".log", sep=""))

    if ( length( grep( "Failed build dependencies", rpmlog, fixed=TRUE)) >0 ) { ### missing dependencies, no chance
        cat( "The following dependencies must be installed to build R-", packname, "\n", sep="")
        for (line in rpmlog) if ( grepl( "needed", line, fixed=TRUE)) cat( line,"\n")
        stop("Missing dependencies!")
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
    system(buildcommand )

    rpmlog <- readLines( paste( specfile, ".log", sep=""))
    if (length( grep( "Wrote:", rpmlog, fixed=TRUE)) == 2) {
        cat("Successfully created rpm package\n")
        print(rpmlog[grep("Wrote:", rpmlog, fixed=TRUE)])
    }
    else {
        stop("Some unknown error, probably external libraries missing as dependencies in spec")
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
