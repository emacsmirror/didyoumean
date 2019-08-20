;;; didyoumean.el --- did you mean to open another file? -*- lexical-binding: t; -*-

;; Authors: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; URL: https://git.sr.ht/~kisaragi_hiu/didyoumean.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; didyoumean.el is free software: you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:
;; A simple function to look up words on the web.
;; Probably better off to just use builtin `webjump.el', but oh well.
;;; Code:

(require 'cl-lib)

(defgroup didyoumean nil
  "Did you mean to open another file?"
  :group 'convenience
  :prefix "didyoumean-")

(defvar didyoumean--history nil "History for `didyoumean' prompts.")

(defun didyoumean ()
  "Shut up."
  (let ((matching-files (cl-remove-if-not
                         (lambda (x) (and (string-prefix-p buffer-file-name x)
                                          (not (equal buffer-file-name x))))
                         (directory-files "." :full))))
    (when matching-files
      (completing-read "Did you mean: "
                       matching-files nil nil nil
                       'didyoumean--history))))

(provide 'didyoumean)
;;; didyoumean.el ends here
