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

(defun didyoumean--matching-files (file)
  "Return files that seems to be similar in name to FILE (including itself)."
  (cl-remove-if-not
   (lambda (x) (and (string-prefix-p file x)))
   (directory-files "." (file-name-absolute-p file))))

;;;###autoload
(cl-defun didyoumean ()
  "Prompt for files similar to the current file if they exist."
  (interactive)
  ;; use early return instead of nesting (when (let (when (let))))
  (unless buffer-file-name (cl-return-from didyoumean nil))
  ;; Switching to the right file if needed
  (let* ((matching-files (didyoumean--matching-files buffer-file-name))
         (comp-read-func
          (cond ((or (bound-and-true-p ivy-mode)
                     (bound-and-true-p helm-mode))
                 ;; `completing-read' would be advised in this case
                 #'completing-read)
                ;; the list needs to be visible up front
                (t (require 'ido)
                   #'ido-completing-read)))
         (correct-file
          (if (> (length matching-files) 1)
              (funcall comp-read-func
                       "Did you mean: "
                       matching-files nil nil nil
                       'didyoumean--history)
            buffer-file-name)))
    (unless (equal buffer-file-name correct-file)
      (find-file correct-file)
      ;; killing the buffer during find-file-hook, sure...?
      (kill-current-buffer))))

;;;###autoload
(add-hook 'find-file-hook #'didyoumean)

(provide 'didyoumean)
;;; didyoumean.el ends here