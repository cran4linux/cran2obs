#' expandSpecForDevel takes a specfile and adds a -devel section
#'
#' @param specfile A spec file
#' @param mainfiles character vector with entries for the files section
#' @param develfiles character vector with entries for the files -devel section
#'
#' @return list of status "done" or "fail" and value, the specfile or NA
#'
#' @export
expandSpecForDevel <- function(specfile, mainfiles, develfiles){
    specLines <- readLines(specfile)
    split <- grep("%description", specLines)

    ## add %package devel
    specLines <- c(specLines[1:(split-1)],
                   c("",
                     "%package        devel",
                     "Summary:        Development files for %{packname}",
                     "Requires:       %{name} = %{version}",
                     "Requires:       R-base-devel",
                     ""),
                   specLines[split:length(specLines)])

    split <- grep("%prep", specLines)

    ## add %description devel
    specLines <- c(specLines[1:(split-1)],
                   c("%description     devel",
                     "Development files and header needed to build packages using %{packname}",
                     ""),
                   specLines[split:length(specLines)])

    ## rewrite %files and %files devel
    split <- grep("%files", specLines)-1
    speclines <- c(specLines[1:split],
                   mainfiles,
                   "",
                   "%files devel",
                   develfiles,
                   "",
                   "%changelog")
    
    writeLines(speclines, con=specfile)
    
    return(list(status="done", value=specfile))
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

#' extractDevelFilesFromLog takes the lines of an osc build log and
#' extrats the list of files which need to be put in a devel packages.
#' Times must be stripped already.
#'
#' @param buildlog Character vector containing the lines of an osc
#' build log
#'
#' @return Character vector containing entries for the files section of
#' a -devel package
#'
#' @export

extractDevelFilesFromLog <- function(buildlog, pkg, systemRlib="/usr/lib64/R/library/"){
    buildlog <- buildlog[ grep( "devel-file-in-non-devel-package", buildlog )]
    buildlog <- gsub( paste0("R-",pkg,".x86_64: E: devel-file-in-non-devel-package \\(Badness: 50\\) ",systemRlib,pkg,"/"), "", buildlog)
    ## cleanup, so just filenames below pkg remain in buildlog

    filelist <- c()
    dirlist <- NULL
    for (line in buildlog){
        if ( grepl( "/", line, fixed=TRUE) ) { ## a file in a subdirectory, extract the unique directories
            nextdir <- unlist( strsplit( line, "/") )[1]
            if (! nextdir %in% dirlist){
                dirlist <- c( dirlist, nextdir )
                filelist <- c( filelist, paste0("%{rlibdir}/%{packname}/", nextdir))
            }
        } else { ## a file
            filelist <- c( filelist, paste0( "%{rlibdir}/%{packname}/", line))
        }
    }
    return(filelist)
}

#' extract the lines off of the %files section of a simple, i.e. non-devel
#' specfile
#' @param specfile is a specfile
#'
#' @return vector of entries of %files section
#'
#' @export
extractFilesFromSimpleSpec <- function(specfile){
    specLines <- readLines(specfile)
    files <- specLines[ grep("%files", specLines):(length(specLines)-2) ]
    return(files)
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
        speclines[filesLine+1] <- "%dir %{rlibdir}/%{packname}"
        ## keep the changelog section
        speclines[filesLine+2] <- ""
        speclines[filesLine+3] <- "%changelog"
    }
    return(list(status="done", speclines=speclines[1:(filesLine+3)]))
}

#' sysreq2depends generats Requires and BuildRequires
#' from SystemRequirements: lines in DESCRIPTION
#'
#' @param line the line from DESCRIPTION
#'
#' @return list with components 'sysdepends' and 'sysbuilddepends'
#'
#' @export
sysreq2depends <- function(line){
    result <- list( depends="", builddepends="")
    addtoresult <- function( r, a, b) { return( list( depends=paste(r$depends, a), builddepends=paste(r$builddepends, b)  ))}
    
    if ( grepl( "ICU4C", line)) {
        result <- addtoresult(result, "icu", "libicu-devel")
    }

    if ( grepl( "git2r", line)){
        result <- addtoresult( result, "libgit2 openssl libssh2", "libgit2-devel zlib-devel openssl-devel libssh2-devel")
    }

    if ( grepl( "glpk", line)){
        result <- addtoresult( result, "glpk", "glpk-devel")
    }
    
    if ( grepl( "gmp", line)){
        result <- addtoresult( result, "", "libgmp-devel")
    }

    if ( grepl( "libcurl", line)){
        result <- addtoresult( result, "", "libcurl-devel")
    }

    if ( grepl( "libjpeg", line)){
        result <- addtoresult( result, "", "libjpeg8-devel")
    }

    if ( grepl( "libpng", line)) {
        result <- addtoresult( result, "", "libpng16-devel libpng16-compat-devel")
    }

    if ( grepl( "libxml2", line)) {
        result <- addtoresult( result, "", "libxml2-devel")
    }

    if ( grepl( "OpenSSL", line)) {
        result <- addtoresult( result, "openssl", "openssl-devel")
    }
    
    if ( grepl( "zlib", line)) {
        result <- addtoresult( result, "", "zlib-devel")
    }

    result
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
                            statusfile = getOption("c2o.statusfile"),
                            log = getOption("c2o.logfile")) {
                
    logger(paste0("** Creating empty spec for pkg ", pkg))
    status <- read.table(statusfile, sep=";", header=TRUE, colClasses="character")
    
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

    if ( any( grep( "SystemRequirements:", description))) {
        logger( paste0( pkg, " has non-empty SystemReqirements"))
        logger( description[ grep( "SystemRequirements", description)])
        sysreqs <- sysreq2depends( description[ grep( "SystemRequirements", description) ]  )
    } else {
        sysreqs <- list(depends="", builddepends="")
    }
    
    if ( any(grep("Encoding: ", description, fixed=TRUE))) {
        ## some DESCRIPTIONs seem to be encoded differently, i.e leerSIECyL
        encoding <- trimws( gsub( "Encoding: ", "",
                                 description[ grep( "Encoding: ", description, fixed=TRUE)]), which="both")
        if (encoding %in% c("latin1", "latin2")) {
            ##            system2( "recode", args=c( "..UTF-8", desc.file))
            ## re-read re-encoded DESCRIPTION
            ##description <- readLines(desc.file)
            description <- iconv(description, encoding, "UTF-8")
        }
    }

    unlink( pkg, recursive=TRUE)
    
    ## template and description have been read in now populate the specfile tempalte
    
    spectpl <- gsub( "{{year}}", format( Sys.time(), "%Y" ), spectpl, fixed=TRUE)    
    spectpl <- gsub( "{{packname}}", pkg, spectpl, fixed=TRUE)
    spectpl <- gsub( "{{version}}", version, spectpl, fixed=TRUE)	
    
    summary.str <- sub( "Title: ", "", description[ grep("Title:", description) ], fixed=TRUE)
    spectpl <- gsub( "{{summary}}", summary.str, spectpl, fixed=TRUE)
    
    license <- sub( "License: ", "", description[ grep("License:", description) ], fixed=TRUE)
    spectpl <- gsub( "{{license}}", license, spectpl, fixed=TRUE)
    
    spectpl <- gsub( "{{source0}}", source0, spectpl, fixed=TRUE)
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
            }
            if ( sysreqs$depends != "") {
                for ( item in strsplit(sysreqs$depends, " ")[[1]]) cat( "Requires:\t", item, "\n", sep="", file=specfile, append=TRUE)
            } else { next }
        } else if ( grepl( "{{builddepends}}", line, fixed=TRUE)) {
            if  ( length( deps) > 0)  {
                for ( item in deps) {
                    if (status$hasDevel[ which( status$Package == gsub("R-", "", item))]) {
                        cat( "BuildRequires: \t", item, "-devel\n", sep="", file=specfile, append=TRUE)
                    } else {
                        cat( "BuildRequires: \t", item, "\n", sep="", file=specfile, append=TRUE)
                    }
                }
            }
            if  ( sysreqs$builddepends != "")  {
                for ( item in strsplit(sysreqs$builddepends, " ")[[1]]) {
                    cat( "BuildRequires: \t", item, "\n", sep="", file=specfile, append=TRUE)
                }
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

