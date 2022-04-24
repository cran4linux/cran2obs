#' addPost adds sections %post and %postun for
#' included libraries
#' 
#' @param specfile a specfile
#'
#' @return list of status "done" or "fail" and value, the specfile or NA
#'
#' @export
addPost <- function( specfile ){
    specLines <- readLines(specfile)
    split <- grep("fdupes -s", specLines, fixed=TRUE)-1
    specLines <- c( specLines[1:split],
                   "", "%post", "/sbin/ldconfig", "", "%postun", "/sbin/ldconfig",
                   specLines[(split+1):length(specLines)]
                   )
    writeLines(specLines, specfile)
    return(list(status="done", value=specfile))
}

#' rmExeFromDoc removes wrongly set exe bits from
#' documentation files
#' 
#' @param specfile a specfile
#' @param exefiles vector of fq-filenames
#'
#' @return list of status "done" or "fail" and value, the specfile or NA
#'
#' @export
rmExeFromDoc <- function( specfile, exefiles ){
    specLines <- readLines(specfile)
    split <- grep("fdupes -s", specLines, fixed=TRUE)-1
    exefiles <- gsub("/usr/lib64/R/library/", "", exefiles)
    exefiles <- sapply( strsplit(exefiles, "/"), function(x) paste( x[2:length(x)], sep="", collapse="/"))
    exefiles <- paste("chmod 644 %{buildroot}%{rlibdir}/%{packname}/", exefiles, sep="")
    specLines <- c( specLines[1:split],
                   exefiles,
                   specLines[(split+1):length(specLines)]
                   )
    writeLines(specLines, specfile)
    return(list(status="done", value=specfile))
}

#' addMakevarsToSpec adds a line of CFLAGS, if needed
#'
#' @param specfile a specfile
#' @param makevar what to add to ~/.R/Makevars in specfile
#'
#' @return list of status "done" or "fail" and value, the specfile or NA
#'
#' @export
addMakevarsToSpec <- function( specfile, makevar){
    specLines <- readLines(specfile)
    split <- grep("rm -rf ~/.R", specLines, fixed=TRUE)
    specLines <- c( specLines[1:split],
                   makevar,
                   specLines[(split+1):length(specLines)]
                   )
    writeLines(specLines, con=specfile)
    return(list(status="done", value=specfile))     
}

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
    addtoresult <- function( r, a, b) { return( list( depends=trimws(paste(r$depends, a)), builddepends=trimws(paste(r$builddepends, b))  ))}

    if ( grepl( "cmake", line)) {
        result <- addtoresult(result, "", "cmake")
    }
    
    if (grepl( "fontconfig", line)){
       result <- addtoresult( result, "fontconfig", "fontconfig fontconfig-devel")   
    }

    if (grepl( "freetype2", line)){
       result <- addtoresult( result, "", "freetype2-devel")   
    }

    if ( grepl( "fftw", line)) {
        result <- addtoresult( result, "", "fftw3-devel")
    }

    if ( grepl( "GDAL", line)) {
        result <- addtoresult( result, "gdal", "gdal gdal-devel")
    }

    if ( grepl( "GEOS", line)) {
        result <- addtoresult( result, "", "geos-devel")
    }

    if ( grepl( "glpk", line)){
        result <- addtoresult( result, "glpk", "glpk-devel")
    }

   if ( grepl( "GLU", line)){
        result <- addtoresult( result, "", "freeglut-devel xorg-x11-devel")
    }
    
    if ( grepl( "gmp", line)){
        result <- addtoresult( result, "libgmp10", "gmp-devel")
    }

    if ( grepl( "Gnu Scientific Library", line)){
        result <- addtoresult( result, "gsl", "gsl-devel")
    }

    if ( grepl( "ICU4C", line)) {
        result <- addtoresult(result, "icu", "libicu-devel")
    }

    if ( grepl( "ImageMagick ", line, fixed=TRUE)){
        result <- addtoresult( result, "ImageMagick", "")
    }

    if ( grepl( "ImageMagick++", line, fixed=TRUE)){
        result <- addtoresult( result, "", "libMagick++-devel")
    }

    
    if ( grepl( "JAGS", line)){
        result <- addtoresult( result, "jags jags-devel", "jags jags-devel")
    }
    
    if ( grepl( "libcurl", line)){
        result <- addtoresult( result, "", "libcurl-devel")
    }

    if ( grepl( "libgit2", line)){
        result <- addtoresult( result, "", "libgit2-devel")
    }

    if ( grepl( "libjpeg", line)){
        result <- addtoresult( result, "", "libjpeg8-devel")
    }

    if ( grepl( "libpng", line)) {
        result <- addtoresult( result, "", "libpng16-devel libpng16-compat-devel")
    }

    if ( grepl( "librsvg2", line)) {
        result <- addtoresult( result, "", "librsvg-devel")
    }

    if ( grepl( "libSSH2", line)){
        result <- addtoresult( result, "libssh2-1", "libssh2-devel")
    }

    if ( grepl( "libxml2", line)) {
        result <- addtoresult( result, "", "libxml2-devel")
    }

    if ( grepl( "netcdf", line)) {
        result <- addtoresult( result, "netcdf", "netcdf netcdf-devel")
    }

    if ( grepl( "ODBC3", line )) {
        result <- addtoresult( result, "iodbc", "iodbc libiodbc-devel")
   }
    
    if ( grepl( "OpenCL", line)) {
        result <- addtoresult( result, "", "ocl-icd-devel opencl-headers")
    }

    if ( grepl( "OpenSSL", line)) {
        result <- addtoresult( result, "openssl", "openssl-devel")
    }

    if ( grepl( "pandoc", line)) {
        result <- addtoresult( result, "pandoc", "")
    }

    if ( grepl( "perl", line)) {
        result <- addtoresult( result, "perl", "")
    }

    if ( grepl( "PROJ", line)) {
        result <- addtoresult( result, "proj", "proj proj-devel")
    }

    if ( grepl( "sqlite3", line)) {
        result <- addtoresult( result, "sqlite3", "sqlite3-devel")
    }
    
    if ( grepl( "udunits", line)){
        result <- addtoresult( result, "udunits2", "udunits2-devel")
    }

    if ( grepl( "zlib", line)) {
        result <- addtoresult( result, "", "zlib-devel")
    }

#    result$depends <- trimws(result$depends)
#    result$builddepends <- trimws(result$builddepends)
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
    manual.sysreq.pkgs <- c("httpuv", "s2", "sf")
    manual.sysreqs <- list(
        httpuv = list( depends="", builddepends="zlib-devel"),
        s2 = list( depends="openssl", builddepends="openssl-devel"),
        sf = list( depends="proj gdal sqlite3",
                   builddepends="proj proj-devel gdal gdal-devel geos-devel sqlite3-devel")
        ## sf fails to declare its dependency on sqlite3
    )

    
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
    
#    description <- readLines( desc.file)

    desc <- read.dcf(desc.file)[1,] # only one package
    unlink( pkg, recursive=TRUE)
    ## template and description have been read in now populate the specfile tempalte

    
    if ( "Encoding" %in% names(desc)){
        if ( desc["Encoding"] %in% c("latin1", "latin2")) desc <- iconv( desc, desc["Encoding"], "UTF-8")
    }

    if (pkg %in% manual.sysreq.pkgs) {
        sysreqs <- manual.sysreqs[[pkg]]
        logger(paste0("  NOTE: ", pkg, " is using manual sysreqs. Check, if still needed!"))
    } else {
        if ( "SystemRequirements" %in% names(desc)) {
            logger( paste0( pkg, " has non-empty SystemReqirements"))
            logger( desc[ "SystemRequirements"])
            sysreqs <- sysreq2depends( gsub("\n", " ", desc[ "SystemRequirements" ])  )
            cat( sysreqs$depends , "\n")
            cat( sysreqs$builddepends , "\n")
            ## convert to single line for greping
        } else {
            sysreqs <- list(depends="", builddepends="")
        }
    }
    
    spectpl <- gsub( "{{year}}", format( Sys.time(), "%Y" ), spectpl, fixed=TRUE)    
    spectpl <- gsub( "{{packname}}", pkg, spectpl, fixed=TRUE)
    spectpl <- gsub( "{{version}}", version, spectpl, fixed=TRUE)	
    
#    summary.str <- sub( "Title: ", "", description[ grep("Title:", description) ], fixed=TRUE)
    summary.str <- gsub("\n", " ", desc[ "Title"])
    ## Summary must be one line
    spectpl <- gsub( "{{summary}}", summary.str, spectpl, fixed=TRUE)
    
#    license <- sub( "License: ", "", description[ grep("License:", description) ], fixed=TRUE)
    license <- desc[ "License"]
    spectpl <- gsub( "{{license}}", license, spectpl, fixed=TRUE)
    
    spectpl <- gsub( "{{source0}}", source0, spectpl, fixed=TRUE)
    deps <- unlist(strsplit(pkg.info$recDep, " "))
    
    if (length(deps) >0) {
        deps <- paste( "R-", deps, sep="")
    } else {
        deps <- c()
    }

    suggests <- unlist(strsplit(pkg.info$Suggests, " "))
    if (length(suggests) > 0) {
        suggests <- paste( "R-", suggests, sep="")
    } else {
        suggests <- c()
    }
    
#    descstart <- grep( "Description:", description, fixed=TRUE)
#    descend <- descstart + grep( "^[A-Z][a-z]+:", description[ (descstart+1):length(description)])[1] -1
#    description.str <- trimws( paste( description[ descstart:descend ] ), which="both")
#    description.str[1] <- gsub("Description: ", "", description.str[1])
### TODO still needs formating! textwrap for R?
    description.str <- desc[ "Description"]
    
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
                for ( item in suggests) cat( "Suggests:\t", item, "\n",  sep="", file=specfile, append=TRUE)
            } else {next}
        } else if ( grepl( "{{needscompilation}}", line, fixed=TRUE) ) {
            if ( needs.compilation == "yes") cat( "BuildRequires:   gcc gcc-c++ gcc-fortran\n", file=specfile, append=TRUE)
        } else if ( grepl( "{{description}}", line, fixed=TRUE) ) {
                                        #for ( item in description.str) cat( item, "\n", file=specfile, append=TRUE)
            for (line in strwrap( gsub("\n", " ", desc[ "Description"]), 72)) cat( line, "\n", file=specfile, append=TRUE)
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

