#!/usr/bin/env bash

## Note! If script does produce unexpected results, please check
## if the encryption key has changed lately.

## TODO1: We should provide an inner script which takes parameters
## that are hidden from outside: key, mysql password

## TODO2: Find out how to retrieve mysql password from azure

set -euo pipefail

function printUsage {
    echo "Usage: mysql-user LIKESTRING [local|utv|preprod|prod] [COMPANYID]"
    echo "Return id, pseudonym and username matching LIKESTRING"
    echo ""
    echo "Optionally, filter results on users in company with ID COMPANYID."
    echo ""
    echo "Options:"
    echo "  -v          Verbose mode"
    echo "  -h          Help"
}

verbose=""
function vecho {
if [ -n "$verbose" ]; then
    echo "$*"
fi
}

while getopts "hv" arg; do
    case $arg in
	h)
	    printUsage
	    exit 0
	    ;;
	v)
	    verbose=1
	    ;;
	\?)
	    echo "Run $script -h for help" >&2
	    exit 0
	    ;;
    esac
done

verbose=1
vecho "Verbose mode is on"

substring="${2:-}"
if [ -z "$substring" ]; then
    printUsage
    exit 0
fi

vecho substring: $substring

env="${2:-local}"
vecho env: $env

company_id="${3:-}"
vecho company_id: $company_id

az_env="$env"
key="Zl6o599pCTUXiKHhG9YzZIBEkuV0ESdW" # local key only

if [ "$env" == "utv" ]; then
    az_env="test"
fi
vecho az env: $az_env

if [ "$env" != "local" ]; then
    key=$(az keyvault secret show --vault-name kv-skolen-$az_env --name USERDATA-ENCRYPTION-KEY | jq -r .value)
fi
vecho key: $key

sql="
SELECT id, pseudonym, username
FROM (
    SELECT
	id,
        pseudonym,
        CAST(
            AES_DECRYPT(
                FROM_BASE64(username_encrypted),
                '$key',
                NULL,
                'hkdf'
            ) AS CHAR
        ) AS username
    FROM user
"

if [[ -n "$company_id" ]]; then
    sql+=" WHERE company_id = ${company_id}"
fi

sql+="
) u
WHERE username LIKE '%${substring}%';
"
vecho "$sql"

if [ "$env" == "local" ]; then
    mysql -h 127.0.0.1 -P 30100 -u root -pdocker77 ada -e "$sql" --batch --skip-column-names
fi
if [ "$env" == "utv" ]; then
    password=$(op item get --vault "PU - Utviklere" --reveal "MySQL User - PU - UTV" --fields=password)
    mysql -h 127.0.0.1 -P 4306 -u skolentest -p"$password" ada_test -e "$sql" --batch --skip-column-names
fi
if [ "$env" == "preprod" ]; then
    password=$(op item get --vault "PU - Utviklere" --reveal "MySQL User - PU - PreProd" --fields=password)
    mysql -h 127.0.0.1 -P 4306 -u skolenpreprod -p"$password" ada_preprod -e "$sql" --batch --skip-column-names
fi
if [ "$env" == "prod" ]; then
    password=$(op item get --vault "PU - Utviklere" --reveal "MySQL User - PU - Prod" --fields=password)
    mysql -h 127.0.0.1 -P 4306 -u skolenprod -p"$password" ada_prod -e "$sql" --batch --skip-column-names
fi

vecho $password
