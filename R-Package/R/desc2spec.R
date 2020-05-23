#' createOrUpdate checks if a package already exists either as local
#' OBS pac or remote pac on build.opensuse.org
#'
#' @param packname CRAN R package name
#' @param remoteproj Existing project to buid packages for in OBS (build.opensuse.org)
#'
#' @return Version number of package in OBS or "missing" 
#' @export

statusOBS <- function(packname, remoteproj="home:dsteuer:AutomaticCRAN/"){
    suppressWarnings(
        osclist <- system2("osc", args=c("ls", paste0(remoteproj,"R-",packname)), stdout=TRUE, stderr=TRUE)
    )
    if ( is.null( attributes( osclist))) {
        return( versionFromTgz(packname, osclist[1]) )
    } else {
        return("missing")
    }
}

#' versionFromTgz extracts the version info, normalizes it and return
#' a charater string containing that version
#'
#' @param packname CRAN package name
#' @param tgzname filename to extract version from
#'
#' @return Version string
#' export

versionFromTgz <- function(packname, tgzname){
    gsub( ".tar.gz", "", gsub( paste0( packname, "-"), "", tgzname))
}

#' createEmptySpec takes the name of an R package and creates an specfile
#' with empty file section from its DESCRIPTION file.
#'
#' @param packname A package name of a package in a R repo like CRAN
#' @param targetdir The directory where rpmbuild should do its work.
#' Sources of packages reside in rpmbuildroot/SOURCES, SPECS in rpmbuildroot/SPECS
#' @param download.cache Directory where sources are stored
#' @param ap A dataframe like the result of available.packages() or cleanDeps()
#' createEmptySpec uses Package, Version, and NeedsCompilation of that dataframe.
#' 
#' @return The path to the generated specfile, if creation succeeded,
#' a character string beginning with Failed: in case of an error.
#' 
#' @export

createEmptySpec <- function(packname, pacdir=paste0("~/steuer/OBS/","home:dsteuer:AutomaticCRAN/R-",packname),
                            download.cache="~/rpmbuild/SOURCES",
                            ap = data.frame(available.packages(repos="https://cloud.r-project.org"))) {
    errorlog <- "desc2spec.err"
    cat("Building ", packname, "\n", file=errorlog, append=TRUE)
    
    if (! file.exists(system.file("specfile.tpl", package="CRAN2OBS"))){
        stop("Specfile template not found. Exit.")
    }
    
    spectpl <- readLines(system.file("specfile.tpl", package="CRAN2OBS"))
    
    if (! packname %in% ap[,"Package"]) {
        cat("Seems ", packname, " no longer exists on CRAN\n")
        cat("Seems ", packname, " no longer exists on CRAN\n", file=errorlog, append=TRUE)
        return(paste("Failed: Package", packname, " no longer on CRAN"))
    }
    
    version <- ap[ap$Package == packname, "Version"]
    
    source0 <-  paste0(packname,"_",version,".tar.gz")
    specfile <- paste0(pacdir,"/R-",packname,".spec")
    
    version <- gsub("-",".",ap[ap$Package == packname,"Version"])
    ## version must be normalized for rpm
    #rpmname <- paste(rpmbuildroot,"RPMS/x86_64/R-", packname,"-",version,"-0.rpm", sep="")
    
    if (! file.exists( file.path(download.cache,source0))) {
        if ( download.file(paste0("https://cloud.r-project.org/src/contrib/", source0),
                            paste0( download.cache, "/", source0)) != 0){
            cat("Seems ", packname, " no source on CRAN\n")
            cat("Seems ", packname, " no source on CRAN\n", file=errorlog, append=TRUE)
            return(paste("Failed: Package", packname, " download failed from CRAN"))
        }
    }
    if (! file.copy(file.path(download.cache,source0), file.path(pacdir,source0))){
        cat("Seems ", packname, " setup of pac failed\n")
        cat("Seems ", packname, " setup of pac failed\n", file=errorlog, append=TRUE)
        return(paste("Failed: Package", packname, " setup of pac failed"))
    }
    
    
### extract DESCRIPTION file, read it, erase it
    desc.file <- paste(packname,"/DESCRIPTION",sep="")
    untar( file.path(pacdir,source0), desc.file )
    
    description <- readLines(desc.file)
    
    if ( length(grep("Encoding: ", description, fixed=TRUE) > 0)) {
        ## some DESCRIPTIONs seem to be encoded differently, i.e leerSIECyL
        encoding <- trimws( gsub("Encoding: ","",description[grep("Encoding: ", description, fixed=TRUE)]), which="both"  )
        if (encoding %in% c("latin1", "latin2")) {
            system2("recode", args=c("..UTF-8", descfile ) )
            ## re-read re-encoded DESCRITION
            description <- readLines(desc.file) 
        }
    }
    
    unlink(packname,recursive=TRUE)
    ## immediatelay unclutter!
    
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
    
    needs.compilation <- ap[ap$Package == packname, "NeedsCompilation"]
    
    #if (file.exists(specfile)) unlink(specfile)  ## We generate a new file! This is not update!
    cat("# Automatically generated by CRAN2OBS::createOBSpac\n", file=specfile)
    
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
    return(specfile)
}

#' createOBSspec takes the name of an R package and creates a 
#' spec file to be used in OBS.
#' The external tool osc is used to build
#' the OpenSUSE package. The resulting spec file will reside in a
#' package dir under a local checkout of a remote proj.
#' The %check section is deliberately empty. We rely
#' on the checks on CRAN.
#'
#' @param packname A package name of a package in a R repo like CRAN
#' @param rpmbuildroot The directory where rpmbuild should do its work
#' @param localOBSdir The top level directory of a checkout of the project
#' you want to create your package in. 
#' @param remoteproj Name of the OBS project
#' @param ap A dataframe containing info like from available.packages or cleanDeps.
#' 
#' @return The path to the generated specfile.
#' @export

createOBSpac <- function(packname, localOBSdir="~/OBS",remoteproj="home:dsteuer:AutomaticCRAN",
                         ap = data.frame(available.packages(repos="https://cloud.r-project.org"))) {
    
    log <- "createOBSpac.log"
    cat("Building ", packname, "\n", file=log, append=TRUE)

    if (! packname %in% ap[,"Package"]) {
        cat("Seems ", packname, " does not exist on CRAN\n")
        cat("Seems ", packname, " does not exist on CRAN\n", file=log, append=TRUE)
        return(paste("Failed: Package", packname, " not on CRAN"))
    }
    
    obsstatus <- statusOBS(packname, remoteproj)
    if (obsstatus != "missing") {
        cat("Failed: ", packname, " exists in OBS. createOBSpac does no updates!", file=log, append=TRUE)
        return("Failed: createOBSpac does no OBS updates!")
    }

    pacdir <-  file.path( localOBSdir, remoteproj, paste0( "R-", packname))

    if ( dir.exists( pacdir )){
        cat("Failed: ", packname, " exists in local OBS. createOBSpac does no updates!", file=log, append=TRUE)
        return("Failed: createOBSpac does no local updates!")
    } else { ## create dir to hold package for OBS
        cmd <- paste("\"", "cd", file.path( localOBSdir, remoteproj), " ; osc mkpac ",paste0( "R-", packname)  , "\"")
        result <- system2(  "bash",  args = c("-c", cmd), stdout=TRUE, stderr=TRUE)
        if( ! is.null(attributes(result))) {
            cat(result)
            cat(packname, " could not setup packname\n")
            cat(packname, " could not setup packname\n", file=log, append=TRUE)
            return(paste("Failed: Package", packname, " could not setup pacdir"))
        }
    }
    
    result <- createEmptySpec(packname, pacdir, download.cache="~/rpmbuild/SOURCES" , ap)
    if (! grepl(".spec", result)){ ##failure
        cat("Failed: ", packname, " no specfile created. Error was ", specfile, "\n", file=log, append=TRUE)
        return("Failed: could not create specfile")
    }
    specfile <- result
    speclines <- readLines(con=specfile)

### call osc
    cmd <- paste("\""," cd", pacdir, "; osc build --prefer-packages=~/rpmbuild/RPMS/x86_64 --local-package --ccache", specfile, "\"" )
    suppressWarnings(
        buildlog <- system2("bash", args=c("-c", cmd), stdout=TRUE, stderr=TRUE)
        ## the first build will fail by construction.
        ## the output of rpmbuild allows to build the %file section
        ## in the spec
        ## seemingly osc produces no buildlog if build locally?
    )

    ## but we have to look for other errors preventing successful second run
    ## first remove useless time stamps

    buildlog <- trimws(gsub("^\\[.*\\]","",buildlog), which="left")
    
    if ( length( grep( "Failed build dependencies", buildlog, fixed=TRUE)) >0 ) { ### missing dependencies, no chance
        cat( "The following missing dependencies must be installed to build R-", packname, "\n", sep="")
        cat( "The following missing dependencies must be installed to build R-", packname, "\n", sep="", file=log, append=TRUE)
        for (line in rpmlog) if ( grepl( "needed", line, fixed=TRUE)) {
                                 cat( line,"\n")
                                 cat( line,"\n", file=log, append=TRUE)
                                 cat("Failed: Missing dependencies first run rpmbuild\n", file=log, append=TRUE)
                             }
        return(paste0("Failed: ",packname ," Missing dependencies first run rpmbuild"))
    }

    if (length( grep("Bad exit status", buildlog, fixed=TRUE)) > 0 ) {
        cat( "Bad exit status from build R-", packname, "\n", sep="")
        cat( "Bad exit status from build R-", packname, "\n", sep="", file=log, append=TRUE)
        return(paste0("Failed: ", packname, " Bad exit status"))
    }

### At this point something was build. But as the specfile.tpl has an empty file section.
### that part must be constructed from error logs. Therfore we know the error structure
### and where to find the unpackaged files list.

    filelist<- extractFilesFromLog(buildlog)
    speclines <- c(speclines , filelist)
    writeLines(speclines, specfile)
### here the %file section is fully populated

### second build!
    cmd <- paste("\""," cd", pacdir, "; osc build --keep-pkgs=~/rpmbuild/RPMS/x86_64 --prefer-packages=~/rpmbuild/RPMS/x86_64 --local-package --ccache",
                 paste0("R-",packname,".spec"), "\"" )
    buildlog <- system2("bash" , args=c("-c", cmd), stdout=TRUE, stderr=TRUE)

    if (length( grep( "Wrote:", buildlog, fixed=TRUE)) == 2) {
        cat("Success: ", packname, " rpm package created\n")
        cat("Success: ", packname, " rpm package created\n", file=log, append=TRUE)
        print(buildlog[grep("Wrote:", buildlog, fixed=TRUE)])
        for (line in  buildlog[grep("Wrote:", buildlog, fixed=TRUE)]){
            cat(line, "\n", file=log, append=TRUE)
        }
        cat("Success: ", packname, " rpm package created\n", file=log, append=TRUE)
    }
    else {
        cat( "Failed to build ", packname, " in second osc build\n", sep="")
        return("Failed: Unknown error second run osc")
    }
### At this point osc successfully generated a package
### TODO Check, if the rpm -q --provides package command only show expected provdides
#### see R-gdata incident
    return(version)
}

#' dropFileSection removes the filelist from a character array
#' representing a spec file. Keep the first line of the file list.
#'
#' @param speclines character vector with lines holding a specfile
#'
#' @value character vector like the input minus file list
#'
#' @expoprt

dropFileSection <- function(speclines){
    FilesLine <- grep("%files",speclines,fixed=TRUE)
    if (length(begOfFiles) != 1) {
        stop("No or more than one %files section! Don't know what to do.")
    } else {
        speclines[begOfFiles+1]="%dir %{rlibdir}/%{packname}"
    }
    return(speclines[1:(begOfFiles+1)])
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

extractFilesFromLog <- function(buildlog){
    buildlog <- buildlog[ ( grep( "RPM build errors", buildlog, fixed=TRUE)+2):( length(buildlog)-5) ]
    buildlog <- gsub( paste0( "/usr/lib64/R/library/", packname, "/"), "", buildlog)

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
                filelist <- c( filelist, cat( "%doc %{rlibdir}/%{packname}/", line))
            } else if (grepl( "NEWS", line, fixed=TRUE)) {
                filelist <- c( filelist, cat( "%doc %{rlibdir}/%{packname}/", line))
            } else {
                filelist <- c( filelist, cat( "%{rlibdir}/%{packname}/", line))
            }
        }
    }
    return(filelist)
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
desc2spec <- function(packname, rpmbuildroot="~/rpmbuild/", ap = data.frame(available.packages(repos="https://cloud.r-project.org"))) {
    errorlog <- "desc2spec.err"
    cat("Building ", packname, "\n", file=errorlog, append=TRUE)

    specfile <- createEmptySpec(packname, rpmbuildroot, ap)

### call rpmbuild
###  osc build --local-package  --ccache R-abc.data.spec 2>&1 
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
    return(c(packname, version, specfile, rpmname))
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
