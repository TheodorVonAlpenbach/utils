#!/bin/bash

cd ~/.ssh

arg=${1:-cappelendamm}
keyName=id_ed25519_cappelendamm

# No argument: switch to MatsBergstromBouvet
case $arg in
    pf|progfab)
	keyName=id_rsa_pf
	echo "Switching github user to TheodorVonAlpenbach (key name: $keyName)"
	;;
    *)
	echo "Switching github user to CappelenDamm (key name: $keyName)"
esac

cp masterkeys/$keyName ./id_rsa
cp masterkeys/$keyName.pub ./id_rsa.pub
