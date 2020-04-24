#!/bin/bash 
# first argument is the CRAN-name of a new package
# Must:  ~/.oscrc contains valid username for OBS
# R2spec must be installed
# osc must be installed
# call: rp2sp.sh <packname> [setup|build|push]

PACK=$1
# PACK may not already exist in OBS


function setup {
    cd ~/OBS/devel\:languages\:R\:released
    # change into OBS directory

    osc mkpac R-$PACK
    cd R-$PACK
    R2rpm --verbose --debug --no-check --no-suggest -p $PACK
    cp ~/rpmbuild/SPECS/R-$PACK.spec .
    cp ~/rpmbuild/SOURCES/${PACK}*.tar.gz .
    # Testbuild
    osc build R-$PACK.spec
    cd -
}

function build {
    cd ~/OBS/devel\:languages\:R\:released/R-$PACK
    osc build --no-init R-$PACK.spec
    cd -
}

function push {
    # Manual correction of filelist, may be multiple cycles
    # maybe osc --noinit in subsequent runs
    # check R2rpm
    # if finally created
    cd ~/OBS/devel\:languages\:R\:released/R-$PACK
    
    osc add R-$PACK.spec ${PACK}*.tar.gz
    MSG="initial build version ` grep Version: *spec | gawk -- '{print $2}'`"
    osc vc -m "$MSG"
    osc ci -m "$MSG"

    cd -
}

if [ $2 = "setup" ]
then
    setup
fi

if [ $2 = "build" ]
then
    build
fi

if [ $2 = "push" ]
then
    push
fi
