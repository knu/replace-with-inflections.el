;;; replace-names.el --- Provides `query-replace-names-with-inflections'.

;; Copyright (c) 2017 Akinori MUSHA
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; Author: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/replace-names.el
;; Created: 7 Seq 2017
;; Version: 0.1.0
;; Package-Requires: ((string-inflection "1.0.5"))
;; Keywords: matching

;;; Commentary:
;;
;; This package currently provides the following function:
;;
;; * `query-replace-names-with-inflections'
;;
;; Read the docstring for details.
;;
;; Here's my suggested settings:
;;
;;   (define-key search-map "n" 'query-replace-names-with-inflections)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'string-inflection)

(defun replace-names--format-string-like (str model-str)
  "Format STR like MODEL-STR."
  (cond
   ((or (string-inflection-word-p model-str)
        (string-inflection-underscore-p model-str))
    (string-inflection-underscore-function str))
   ((string-inflection-upcase-p model-str)
    (string-inflection-upcase-function str))
   ((string-inflection-camelcase-p model-str)
    (string-inflection-camelcase-function str))
   ((string-inflection-lower-camelcase-p model-str)
    (string-inflection-lower-camelcase-function str))
   ((string-inflection-kebab-case-p model-str)
    (string-inflection-kebab-case-function str))
   (t
    str)))

;;;###autoload
(defun query-replace-names-with-inflections (from-string to-string &optional delimited start end)
  "Interactively replace various forms of FROM-STRING with those of TO-STRING.

Occurences of FROM-STRING in any of underscore, upcase,
camelcase, lower-camelcase or kebab case forms match, and each
replacement will be the same form of TO-STRING as the one
matched.

For example, replacing `foo_bar' with `baz_quux' will also
replace `FooBar' with `BazQuux', `FOO_BAR' with `BAZ_QUUX', and
so on.

Third arg DELIMITED (prefix arg if interactive), if non-nil,
means replace only matches that are surrounded by symbol
boundaries.

Fourth and fifth arg START and END (active region if interactive)
specify the region to operate on."
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Query replace"
		   (if current-prefix-arg " symbol" " name")
		   (if (use-region-p) " in region" ""))
	   nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (if (use-region-p) (region-beginning))
	   (if (use-region-p) (region-end)))))
  (let ((regexp (regexp-opt
                 (mapcar #'(lambda (func) (funcall func from-string))
                         '(string-inflection-underscore-function
                           string-inflection-upcase-function
                           string-inflection-camelcase-function
                           string-inflection-lower-camelcase-function
                           string-inflection-kebab-case-function))
                 (if delimited 'symbols t)))
        (orig-query-replace-descr (symbol-function 'query-replace-descr)))
    (letf (((symbol-function 'query-replace-descr)
            (lambda (string)
              (funcall orig-query-replace-descr
                       (if (string-equal string regexp)
                           (match-string 1) ;; show the matched name instead of the regexp pattern
                         string)))))
      (query-replace-regexp-eval regexp
                                 `(replace-names--format-string-like ,to-string (match-string 1))
                                 nil start end))))

(provide 'replace-names)
;;; replace-names.el ends here
