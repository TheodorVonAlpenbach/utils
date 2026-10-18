;;; test-mb-json.el --- Tests for mb-json -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-json)

(ert-deftest mb-json-decodes-outer-json-string ()
  (with-temp-buffer
    (insert (json-serialize "{\"answer\":42,\"text\":\"line\\\\nnext\"}"))
    (should (json-remove-outer-quotes))
    (should (equal (buffer-string)
                   "{\"answer\":42,\"text\":\"line\\\\nnext\"}")))
  (with-temp-buffer
    (insert "{\"answer\":42}")
    (should-not (json-remove-outer-quotes))
    (should (equal (buffer-string) "{\"answer\":42}")))
  (with-temp-buffer
    (insert "\"unfinished")
    (should-not (json-remove-outer-quotes))))

(ert-deftest mb-json-quotes-bare-iso-times ()
  (with-temp-buffer
    (insert (concat "{\"bare\":2026-10-18T12:34:56.789Z,"
                    "\"whole\":2026-10-18T12:34:56Z,"
                    "\"quoted\":\"2026-10-18T12:34:56Z\","
                    "\"embedded\":\"prefix:2026-10-18T12:34:56Z,suffix\"}"))
    (should (= (json-quote-iso-time) 2))
    (should
     (equal
      (buffer-string)
      (concat "{\"bare\":\"2026-10-18T12:34:56.789Z\","
              "\"whole\":\"2026-10-18T12:34:56Z\","
              "\"quoted\":\"2026-10-18T12:34:56Z\","
              "\"embedded\":\"prefix:2026-10-18T12:34:56Z,suffix\"}"))))
  (with-temp-buffer
    (insert "  2026-10-18T12:34:56Z  ")
    (should (= (json-quote-iso-time) 1))
    (should (equal (buffer-string) "  \"2026-10-18T12:34:56Z\"  "))))

(ert-deftest mb-json-pretty-prints-serialized-json ()
  (with-temp-buffer
    (insert
     (json-serialize
      "{\"date\":2026-10-18T12:34:56.789Z,\"answer\":42}"))
    (json-pretty-print-string)
    (should
     (equal
      (buffer-string)
      (concat "{\n"
              "  \"date\": \"2026-10-18T12:34:56.789Z\",\n"
              "  \"answer\": 42\n"
              "}")))))

(ert-deftest mb-json-converts-mongo-shell-values ()
  (with-temp-buffer
    (insert
     "{\"id\":ObjectId(\"abc123\"),\"date\":ISODate(\"2026-10-18\")}")
    (should (= (object-id-to-string) 2))
    (should
     (equal
      (buffer-string)
      (concat "{\"id\":\"ObjectId:abc123\","
              "\"date\":\"ISODate:2026-10-18\"}")))))

(ert-deftest mb-json-invalid-outer-string-signals ()
  (with-temp-buffer
    (insert "\"not closed\" trailing")
    (should-not (json-remove-outer-quotes)))
  (with-temp-buffer
    (insert "\"{invalid}\"")
    (json-remove-outer-quotes)
    (should-error (json-pretty-print-string))))

(provide 'test-mb-json)

;;; test-mb-json.el ends here
