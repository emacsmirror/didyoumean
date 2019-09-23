;;; didyoumean.el --- Did you mean to open another file? -*- lexical-binding: t; -*-

;; Authors: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; URL: https://gitlab.com/kisaragi-hiu/didyoumean.el
;; Version: 0.3.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; didyoumean.el is free software: you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:
;; Ask for the right file to open.

;;; Code:

(require 'cl-lib)

(defgroup didyoumean nil
  "Did you mean to open another file?"
  :group 'convenience
  :prefix "didyoumean-")

(defcustom didyoumean-ignored-suffixes '(".elc" "~" ".bak" ".backup"
                                         ".pacnew" ".pacsave")
  "Do not suggest files that have these suffixes."
  :group 'didyoumean
  :type '(repeat string))

(defcustom didyoumean-custom-ignore-function nil
  "Do not suggest files that make this function return non-nil."
  :group 'didyoumean
  :type '(choice (const :tag "None" nil)
                 function))

(defvar didyoumean--history nil "History for `didyoumean' prompts.")

(defun didyoumean--matching-files (file)
  "Return files that seems to be similar in name to FILE (excluding itself)."
  (when (stringp file)
    (cl-remove-if
     (lambda (x) (or (not (string-prefix-p file x))
                     (equal file x)
                     ;; don't suggest suffixes in this list
                     (and didyoumean-ignored-suffixes
                          (cl-remove nil (mapcar
                                          (lambda (suf)
                                            (string-suffix-p suf x :ignore-case))
                                          didyoumean-ignored-suffixes)))
                     ;; don't suggest anything that d-c-i-f says so
                     (and (functionp didyoumean-custom-ignore-function)
                          (funcall didyoumean-custom-ignore-function file))))
     (directory-files "." (file-name-absolute-p file)))))

;;;###autoload
(defun didyoumean ()
  "Prompt for files similar to the current file if they exist."
  (interactive)
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
          (when matching-files
            (funcall comp-read-func
                     "Did you mean: "
                     `(,buffer-file-name ,@matching-files)
                     nil nil nil
                     'didyoumean--history)))
         (this-file (current-buffer)))
    (when (and correct-file
               (not (equal buffer-file-name correct-file)))
      (find-file correct-file)
      ;; killing the buffer during find-file-hook, sure...?
      (kill-buffer this-file))))

;;;###autoload
(add-hook 'find-file-hook #'didyoumean)

(defun didyoumean-unload-function ()
  "Unload DidYouMean."
  (remove-hook 'find-file-hook #'didyoumean)
  ;; Continue standard unloading.
  ;; See (info "(elisp)Unloading").
  nil)

(provide 'didyoumean)
;;; didyoumean.el ends here
