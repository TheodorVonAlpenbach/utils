o ISO-tid må være på en av formen
  yyyy-mm-ddThh:mm:ss\(\.[0-9]*\)Z

o Modifikatorer er 

tfind () { echo $SEP; echo Tekstsoek: ; echo $SEP; find . -type f -print | xargs grep -i --color=auto -n "$1" ; }
dfind () { echo $SEP; echo Tekstsoek: ; echo $SEP; find . -type d -print | xargs grep -i --color=auto -n "$1" ; }
tfindn () { echo $SEP; echo Tekstsoek: ; echo $SEP; find . -name "$2" -type f -print | xargs grep -i --color=auto -n "$1" ; }
tfind1 () { echo $SEP; echo Tekstsoek: ; echo $SEP; find . -maxdepth 1 -type f -print | xargs grep -i --color=auto -n "$1" ; }
tfind2 () { echo $SEP; echo Tekstsoek: ; echo $SEP; find . -maxdepth 2 -type f -print | xargs grep -i --color=auto -n "$1" ; }
ffind () { echo $SEP; echo Filsoek: ; echo $SEP; find . -type f -print | grep -i --color=auto $1 2>/dev/null ; }
cdalias () {
    alias $1="cd \"$PWD\"" ;
    alias $1 >> ~/.bashrc ;
}

umask 0002
