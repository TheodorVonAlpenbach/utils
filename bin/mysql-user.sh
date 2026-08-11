#!/usr/bin/env bash

set -euo pipefail

substring="$1"
company_id="${2:-}"

sql="
SELECT pseudonym, username
FROM (
    SELECT
        pseudonym,
        CAST(
            AES_DECRYPT(
                FROM_BASE64(username_encrypted),
                'z4dEE792zSTUq7T1RUkPC086C1Nyfqjt',
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

# local
mysql -h 127.0.0.1 -P 30100 -u root -pdocker77 ada -e "$sql" --batch --skip-column-names
# utv
# mysql -h 127.0.0.1 -P 4306 -u skolentest -ppW5P5E9sC78qBXwVgg ada_test -e "$sql" --batch --skip-column-names
# preprod
# mysql -h 127.0.0.1 -P 4306 -u skolentest -ppW5P5E9sC78qBXwVgg ada_test -e "$sql" --batch --skip-column-names
# prod
# mysql -h 127.0.0.1 -P 4306 -u skolentest -ppW5P5E9sC78qBXwVgg ada_test -e "$sql" --batch --skip-column-names
