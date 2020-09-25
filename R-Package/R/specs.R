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
desc2spec <- function(packname, rpmbuildroot="~/rpmbuild/",
                      ap = data.frame(available.packages(repos="https://cloud.r-project.org"))) {
    errorlog <- "desc2spec.err"
    cat("Building ", packname, "\n", file=errorlog, append=TRUE)

    specfile <- createEmptySpec(packname, rpmbuildroot, ap)

### call rpmbuild

    suppressWarnings(
        rpmlog <- system2("rpmbuild", 
                          args   = c("-ba", specfile),
                          env    = "LANG=en;",
                          stdout = TRUE,
                          stderr = TRUE)
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
                                 cat("Failed: Missing dependencies first run rpmbuild\n", file=errorlog, append=TRUE)
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
    
    # Double sub-setting needed because "Installed (but unpackaged) file(s) found" isn't
    # necessarily right under "RPM build errors"...
    rpmlog <- rpmlog[ (grep( "RPM build errors", rpmlog, fixed=TRUE)+1):length(rpmlog)]
    rpmlog <- trimws( rpmlog[ (grep( "Installed (but unpackaged) file(s) found", rpmlog, fixed=TRUE)+1):length(rpmlog)], which="left")
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
    rpmlog <- system2("rpmbuild", 
                      args   = c("-ba", specfile),
                      env    = "LANG=en;",
                      stdout = TRUE,
                      stderr = TRUE)
    
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
    return(c(packname, version, specfile, rpmname))
}


#' extractFilesFromLog takes the lines of an osc build log and
#' extrats the list of unpackaged files. Times must be stripped
#' already.
#'
#' @param buildlog Character vector containing the lines of an osc
#' build log
#'
#' @return Character vector containing entries for the files section,
#'
#' @export

extractFilesFromLog <- function(buildlog, packname, systemRlib="/usr/lib64/R/library/"){
    buildlog <- buildlog[ ( grep( "RPM build errors", buildlog, fixed=TRUE)+2):( length(buildlog)-5) ]
    buildlog <- gsub( paste0( systemRlib, packname, "/"), "", buildlog)

    filelist <- c()
    dirlist <- NULL
    for (line in buildlog){
        if ( grepl( "/", line, fixed=TRUE) ) { ## a file in a subdirectory, extract the unique directories
            nextdir <- unlist( strsplit( line, "/") )[1]
            if (! nextdir %in% dirlist){
                dirlist <- c( dirlist, nextdir )
                if (grepl( "html", line, fixed=TRUE) | grepl( "help", line, fixed=TRUE)) {
                    filelist <- c( filelist, paste0("%doc %{rlibdir}/%{packname}/", nextdir))
                } else {
                    filelist <- c( filelist, paste0("%{rlibdir}/%{packname}/", nextdir))
                }
            }
        } else { ## a file
            if ( grepl( "LICEN", line, fixed=TRUE)) {
                filelist <- c( filelist, paste0( "%license %{rlibdir}/%{packname}/", line))
            } else if (grepl( "DESCRIPTION", line, fixed=TRUE)) {
                filelist <- c( filelist, paste0( "%doc %{rlibdir}/%{packname}/", line))
            } else if (grepl( "NEWS", line, fixed=TRUE)) {
                filelist <- c( filelist, paste0( "%doc %{rlibdir}/%{packname}/", line))
            } else {
                filelist <- c( filelist, paste0( "%{rlibdir}/%{packname}/", line))
            }
        }
    }
    return(filelist)
}


#' dropFileSection removes the filelist from a character array
#' representing a spec file. Keep the first line of the file list.
#'
#' @param speclines character vector with lines holding a specfile
#'
#' @return list of two components status and speclines
#' in status is "success" oder "fail", speclines either with empty file
#' section resp. unaltered.
#'
#' @export

dropFileSection <- function(speclines){
    filesLine <- grep("%files",speclines,fixed=TRUE)
    if (length(filesLine) != 1) {
        return(list(status="fail", speclines, problem="No or more than one %files section! Don't know what to do."))
    } else {
        speclines[filesLine+1]="%dir %{rlibdir}/%{packname}"
    }
    return(list(status="done", speclines=speclines[1:(filesLine+1)]))
}


#' createEmptySpec takes the name of an R package and creates an specfile
#' with empty file section from its DESCRIPTION file. The OBS directory must
#' be prepared by setupOBSdir
#'
#' @param pkg A package name of a package in a R repo like CRAN
#' @param targetdir The directory where rpmbuild should do its work.
#' Sources of packages reside in rpmbuildroot/SOURCES, SPECS in rpmbuildroot/SPECS
#' @param download.cache Directory where sources are stored
#' @param status A dataframe like the result of available.packages() or cleanDeps()
#' createEmptySpec uses Package, Version, and NeedsCompilation of that dataframe.
#' 
#' @return list of status, 'fail' and 'done' and value. 'value' is either the filename of the generated specfile
#' or a problem description
#' 
#' @export
createEmptySpec <- function(pkg,
                            pac=file.path(getOption("c2o.localOBSdir"), getOption("c2o.auto"), paste0("R-",pkg)),
                            download.cache=getOption("c2o.download.cache"),
                            cran=getOption("c2o.cran"),
                            status = getOption("c2o.status"),
                            log = getOption("c2o.logfile")) {
                
    logger(paste0("** Creating empty spec for pkg ", pkg))
    
    if (! file.exists(system.file("specfile.tpl", package="CRAN2OBS"))){
        logger("*** specfile template not found ?!")
        return(list(status="fail", value="Specfile template not found."))
    }
    
    spectpl <- readLines(system.file("specfile.tpl", package="CRAN2OBS"))
        
    pkg.info <- status[ status$Package == pkg, ]
    
    source0 <-  paste0(pkg,"_",pkg.info$Version,".tar.gz")
    specfile <- paste0(pac,"/R-",pkg,".spec")
    
    version <- gsub("-",".", pkg.info$Version)
    ## version must be normalized for rpm
    
    ## extract DESCRIPTION file, read it, erase it
    desc.file <- paste(pkg, "/DESCRIPTION", sep="")
    untar( file.path( pac, source0), desc.file)
    
    description <- readLines( desc.file)
    
    if ( any(grep("Encoding: ", description, fixed=TRUE))) {
        ## some DESCRIPTIONs seem to be encoded differently, i.e leerSIECyL
        encoding <- trimws( gsub( "Encoding: ", "",
                                 description[ grep( "Encoding: ", description, fixed=TRUE)]), which="both")
        if (encoding %in% c("latin1", "latin2")) {
            system2( "recode", args=c( "..UTF-8", desc.file))
            ## re-read re-encoded DESCRITION
            description <- readLines(desc.file) 
        }
    }

    unlink( pkg, recursive=TRUE)
    
    ## template and descrition have been read in now populate the specfile tempalte
    
    spectpl <- gsub( "{{year}}", format( Sys.time(), "%Y" ), spectpl, fixed=TRUE)    
    spectpl <- gsub( "{{packname}}", pkg, spectpl, fixed=TRUE)
    spectpl <- gsub( "{{version}}", version, spectpl, fixed=TRUE)	
    
    summary.str <- sub( "Title: ", "", description[ grep("Title:", description) ], fixed=TRUE)
    spectpl <- gsub( "{{summary}}", summary.str, spectpl, fixed=TRUE)
    
    license <- sub( "License: ", "", description[ grep("License:", description) ], fixed=TRUE)
    spectpl <- gsub( "{{license}}", license, spectpl, fixed=TRUE)
    
    spectpl <- gsub( "{{source0}}", source0, spectpl, fixed=TRUE)
#    deps <-  cleanList( packname, "depends" )
    deps <- unlist(strsplit(pkg.info$recDep, " "))
    
    if (length(deps) >0) {
        deps <- paste( "R-", deps, sep="")
    } else {
        deps <- c()
    }

#    suggests <- cleanList( packname, "suggests")
    suggests <- unlist(strsplit(pkg.info$Suggests, " "))
    if (length(suggests) > 0) {
        suggests <- paste( "R-", suggests, sep="")
    } else {
        suggests <- c()
    }
    
    descstart <- grep( "Description:", description, fixed=TRUE)
    descend <- descstart + grep( "^[A-Z][a-z]+:", description[ (descstart+1):length(description)])[1] -1
    description.str <- trimws( paste( description[ descstart:descend ] ), which="both")
    description.str[1] <- gsub("Description: ", "", description.str[1])
### TODO still needs formating! textwrap for R?
    
    needs.compilation <- pkg.info$NeedsCompilation
    
    cat( "# Automatically generated by CRAN2OBS\n", file=specfile)
    
    for (line in spectpl) {
        if ( grepl( "{{depends}}", line, fixed=TRUE)) {
            if ( length( deps) > 0) {
                for ( item in deps) cat( "Requires:\t", item, "\n", sep="", file=specfile, append=TRUE)
            } else { next }
        } else if ( grepl( "{{builddepends}}", line, fixed=TRUE)) {
            if  ( length( deps) > 0)  {
                for ( item in deps) cat( "BuildRequires:\t", item, "\n", sep="", file=specfile, append=TRUE)
            } else { next }
        } else if ( grepl( "{{suggests}}", line, fixed=TRUE) ) {
            if ( length( suggests) > 0)  {
                for ( item in suggests) cat( "Recommends:\t", item, "\n",  sep="", file=specfile, append=TRUE)
            } else {next}
        } else if ( grepl( "{{needscompilation}}", line, fixed=TRUE) ) {
            if ( needs.compilation == "yes") cat( "BuildRequires:   gcc gcc-c++ gcc-fortran\n", file=specfile, append=TRUE)
        } else if ( grepl( "{{description}}", line, fixed=TRUE) ) {
            for ( item in description.str) cat( item, "\n", file=specfile, append=TRUE)
        } else {
            cat( line, "\n", file=specfile, append=TRUE)
        }
    }
    return(list(status="done", value=specfile))
}
