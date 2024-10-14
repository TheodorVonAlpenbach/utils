#!/bin/bash

cd ~/.ssh

arg="${1:-cappelendamm}"
algorithm="ed25519"
prefix="id_${algorithm}"
keyName="${prefix}_cappelendamm"

# No argument: switch to MatsBergstromBouvet
case $arg in
    pf|progfab)
	keyName="id_${algorithm}_pf"
	echo "Switching github user to TheodorVonAlpenbach (key name: $keyName)"
	;;
    -h|--help)
	echo "Usage: githubswitchuser [id_ed25519_cappelendamm|id_ed25519_pf]"
	echo
	echo "Without argument, the default id_ed25519_cappelendamm is applied."
	echo "For arguments pf|progfab|id_ed25519_pf, id_ed25519_pf is applied."
	echo "For all other arguments (except -h and --help) the default is applied."
	;;
    *)
	echo "Switching github user to CappelenDamm (key name: $keyName)"
esac

cp "masterkeys/$keyName" "$prefix"
cp "masterkeys/$keyName.pub" "$prefix.pub"
