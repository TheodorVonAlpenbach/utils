(setf *access-key*
      "DP4UrYghy/vL4V5ZScDh9w65ms38XLAOQw1LsMauQVONzxwllfVuRR/KrC4pNEfefjQbSzDqJg/KkXLGovN9sQ==")
;;(base64-encode-string *access-key* )
;;(base64-decode-string *access-key*)
;;(base64-decode-string (base64-encode-string *access-key*))
;;(length *access-key*)
(setf *storage-account* "nosdev")
(setf *container-name* "lightstructures")
(setf *blob-name* "hello.txt")
(setf *target* "~/tmp/azuretest/hello.txt")
;;(eighth (file-attributes *target*))


(setf *destination*
      (format "https://%s.blob.core.windows.net/%s/%s"
	*storage-account* *container-name* *blob-name*))

(cl-defun azure-signature-string (verb)
  (format "%s\n\n\n\n\n\n\n\n\n\n\n\nx-ms-date:Fri, 26 Jun 2015 23:39:12 GMT\nx-ms-version:2015-02-21\n/myaccount/mycontainer\nrestype:container\ntimeout:30"
   (sstring verb)))
;;(azure-signature-string :PUT)

(cl-defun azure-encode-signature-string (string key)
  (base64-encode-string
   (hmac (encode-coding-string string 'utf-8 t) key)))
;;(azure-encode-signature-string (azure-signature-string :PUT) "")
(length "ctzMq410TV3wS7upTBcunJTDLEJwMAZuFPfr0mrrA08=")

(cl-defun upload-blob (target storage-account container-name blob-name destination access-key)
  (let* ((current-date (format-time-string "%a, %e %b %Y %H:%M:%S %Z"))
	(file-length (eighth (file-attributes target)))
	(header-resource
	   (format
	       "x-ms-blob-cache-control:max-age=3600\nx-ms-blob-type:BlockBlob\nx-ms-date:%s\nx-ms-version:2015-12-11"
	     current-date))
	(url-resource (format "/%s/%s/%s" storage-account container-name blob-name))
	(lines (list "PUT"                ;HTTP Verb
		     ""                   ;Content-Encoding
		     ""                   ;Content-Language
		     file-length          ;Content-Length (include value when zero)
		     ""                   ;Content-MD5
		     "text/plain"         ;Content-Type
		     ""                   ;Date
		     ""                   ;If-Modified-Since 
		     ""                   ;If-Match
		     ""                   ;If-None-Match
		     ""                   ;If-Unmodified-Since
		     ""                   ;Range
		     header-resource      ;CanonicalizedHeaders
		     url-resource         ;CanonicalizedResource
		     ))
	 (string-to-sign (concat* lines :in "\n"))
	 (signature
	  (base64-encode-string
	   (hash-hmac (encode-coding-string string-to-sign 'utf-8 t)
		      (base64-decode-string access-key))))
	 (auth-header (format "SharedKey %s:%s" storage-account signature))))
    ;;base64_encode(hash_hmac('sha256', urldecode(utf8_encode($str2sign)), base64_decode(access-key), true));  

    $headers = [
        'Authorization: ' . $authHeader,
        'x-ms-blob-cache-control: max-age=3600',
        'x-ms-blob-type: BlockBlob',
        'x-ms-date: ' . current-date,
        'x-ms-version: 2015-12-11',
        'Content-Type: image/png',
        'Content-Length: ' . $fileLen
    ];

)
;;(update-blob *target* *storage-account* *container-name*, *blob-name* *destination* *access-key*)

;; $ch = curl_init(destination);
;; curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);
;; curl_setopt($ch, CURLOPT_SSL_VERIFYHOST, false);
;; curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);
;; curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
;; curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "PUT");
;; curl_setopt($ch, CURLOPT_INFILE, $handle); 
;; curl_setopt($ch, CURLOPT_INFILESIZE, $fileLen); 
;; curl_setopt($ch, CURLOPT_UPLOAD, true); 
;; $result = curl_exec($ch);

;; echo ('Result<br/>');
;; print_r($result);

;; echo ('Error<br/>');
;; print_r(curl_error($ch));

;; curl_close($ch);
(cl-defun sha256 (string) (secure-hash 'sha256 string))
;;(sha256 "")
(cl-defun sha1 (string) (secure-hash 'sha1 string))
;;(sha1 "")

