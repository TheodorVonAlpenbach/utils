#!/bin/bash

cd ~/.ssh

arg=${1:-cappelendamm}
keyName=id_ed25519_cappelendamm

# No argument: switch to MatsBergstromBouvet
case $arg in
    pf|progfab|id_rsa_pf)
	keyName=id_rsa_pf
	echo "Switching github user to TheodorVonAlpenbach (key name: $keyName)"
	;;
    -h|--help)
	echo "Usage: githubswitchuser [id_ed25519_cappelendamm|id_rsa_pf]"
	echo
	echo "Without argument, the default id_ed25519_cappelendamm is applied."
	echo "For arguments pf|progfab|id_rsa_pf, id_rsa_pf is applied."
	echo "For all other arguments (except -h and --help) the default is applied."
	;;
    *)
	echo "Switching github user to CappelenDamm (key name: $keyName)"
esac

cp masterkeys/$keyName ./id_rsa
cp masterkeys/$keyName.pub ./id_rsa.pub
