#!/usr/bin/env bash

set -euo pipefail

pseudonym="$1"

sql="
SELECT
    id,
    CAST(
	AES_DECRYPT(
	    FROM_BASE64(username_encrypted),
	    'Zl6o599pCTUXiKHhG9YzZIBEkuV0ESdW',
	    NULL,
	    'hkdf'
	) AS CHAR
    ) AS username
FROM user
WHERE pseudonym = '$pseudonym'";

echo $sql

# local
mysql -h 127.0.0.1 -P 30100 -u root -pdocker77 ada -e "$sql" --batch --skip-column-names
# utv
# mysql -h 127.0.0.1 -P 4306 -u skolentest -ppW5P5E9sC78qBXwVgg ada_test -e "$sql" --batch --skip-column-names
# preprod
# mysql -h 127.0.0.1 -P 4306 -u skolentest -ppW5P5E9sC78qBXwVgg ada_test -e "$sql" --batch --skip-column-names
# prod
# mysql -h 127.0.0.1 -P 4306 -u skolentest -ppW5P5E9sC78qBXwVgg ada_test -e "$sql" --batch --skip-column-names
