;;; Return an HMAC authentication code for KEY and MESSAGE using HASH.
;;; 
;;; KEY and MESSAGE must be unibyte strings.  The result is a unibyte
;;; string.  Use the function `encode-hex-string' or the function
;;; `base64-encode-string' to produce human-readable output.
;;;
;;; HASH is a cryptographic hash function, e.g. sha1, sha256, and md5.
;;; It requires one argument, which must be a unibyte string.
;;; 
;;; See URL:<http://en.wikipedia.org/wiki/HMAC> for more information
;;; on the HMAC algorithm.
;;; 
;;; The Emacs multibyte representation actually uses a series of
;;; 8-bit values under the hood, so we could have allowed multibyte
;;; strings as arguments.  However, internal 8-bit values don't
;;; correspond to any external representation \(at least for major
;;; version 22).  This makes multibyte strings useless for generating
;;; hashes.
;;; 
;;; Instead, callers must explicitly pick and use an encoding for
;;; their multibyte data.  Most callers will want to use UTF-8
;;; encoding, which we can generate as follows:
;;; 
;;; (let ((unibyte-key   (encode-coding-string key   'utf-8 t))
;;;       (unibyte-value (encode-coding-string value 'utf-8 t)))
;;; (hmac unibyte-key unibyte-value))
;;; 
;;; For keys and values that are already unibyte, the
;;; `encode-coding-string' calls just return the same string.
;;;
;;; Authors: Derek Upham - sand (at) blarg.net
;;;          Mats Bergstrøm - mbe (at) lightstructures.no
;;;
;;; Copyright: This code is in the public domain.

(cl-defun hmac (key message &optional (hash-function :sha256))
  "Supported values for HASH-FUNCTION are
:sha, :sha1, :mdc2, :ripemd160, :sha224, :sha256, :sha384, :sha512, :md2, :md4, :md5,
and :dss1.
"
  (substring (call-process-shell-command*
	      (format "echo -n \"%s\" | openssl dgst -%s -hmac \"%s\""
		message (downcase (sstring hash-function)) key))
	     9))
;;(hmac "" "")


(cl-defun hmac-emacs (key message hash-function block-size)
  "Return an HMAC code for KEY and MESSAGE using HASH-FUNCTION.

KEY and MESSAGE must be unibyte strings.  The result is a unibyte
string.  Use the function `encode-hex-string' or the function
`base64-encode-string' to produce human-readable output.

HASH-FUNCTION is a cryptographic hash function, e.g. sha1,
sha256, and md5. It requires one argument, which must be a
unibyte string. BLOCK-SIZE is the blocksize of HASH-FUNCTION.

See URL:<http://en.wikipedia.org/wiki/HMAC> for more information
on the HMAC algorithm.

The Emacs multibyte representation actually uses a series of
8-bit values under the hood, so we could have allowed multibyte
strings as arguments.  However, internal 8-bit values don't
correspond to any external representation \(at least for major
version 22).  This makes multibyte strings useless for generating
hashes.

Instead, callers must explicitly pick and use an encoding for
their multibyte data.  Most callers will want to use UTF-8
encoding, which we can generate as follows:

  (let ((unibyte-key   (encode-coding-string key   'utf-8 t))
	(unibyte-value (encode-coding-string value 'utf-8 t)))
    (hmac unibyte-key unibyte-value))

For keys and values that are already unibyte, the
`encode-coding-string' calls just return the same string."
  (when (multibyte-string-p key)
    (error "key %s must be unibyte" key))
  (when (multibyte-string-p message)
    (error "message %s must be unibyte" message))

  ;; The key block is always exactly the block size of the hash
  ;; algorithm.  If the key is too small, we pad it with zeroes (or
  ;; instead, we initialize the key block with zeroes and copy the
  ;; key onto the nulls).  If the key is too large, we run it
  ;; through the hash algorithm and use the hashed value (strange
  ;; but true).

  (when (< block-size (length key))
    (setq key (funcall hash-function key)))

  (let ((key-block (make-vector block-size 0)))
    (dotimes (i (length key))
      (aset key-block i (aref key i)))

    (let ((opad (make-vector block-size #x5c))
	  (ipad (make-vector block-size #x36)))

      (dotimes (i block-size)
	(aset ipad i (logxor (aref ipad i) (aref key-block i)))
	(aset opad i (logxor (aref opad i) (aref key-block i))))

      (when (fboundp 'unibyte-string)
	;; `concat' of Emacs23 (and later?) generates a multi-byte
	;; string from a vector of characters with eight bit.
	;; Since `opad' and `ipad' must be unibyte, we have to
	;; convert them by using `unibyte-string'.
	;; We cannot use `string-as-unibyte' here because it encodes
	;; bytes with the manner of UTF-8.
	(setq opad (apply 'unibyte-string (mapcar 'identity opad)))
	(setq ipad (apply 'unibyte-string (mapcar 'identity ipad))))

      (funcall hash-function
	(concat opad (funcall hash-function
		       (concat ipad message)))))))
;;(hmac "" "" #'sha256 64)

(provide 'hmac)
