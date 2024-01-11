;;; flymake-eldev.el --- Eldev support in Flymake  -*- lexical-binding: t -*-

;;; Copyright (C) 2020-2024 Paul Pogonyshev

;; Author:     Paul Pogonyshev <pogonyshev@gmail.com>
;; Maintainer: Paul Pogonyshev <pogonyshev@gmail.com>
;; Version:    0.9snapshot
;; Keywords:   tools, convenience
;; Homepage:   https://github.com/emacs-eldev/flymake-eldev
;; Package-Requires: ((dash "2.17") (emacs "28.1"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses.

;;; Commentary:

(eval-and-compile
  (require 'flymake)
  (require 'dash))


(defvar flymake-eldev-active t
  "Whether Eldev extension to Flymake is active.")

(defvar flymake-eldev-general-error
  "Eldev cannot be initialized; check dependency declarations and file `Eldev'"
  "Error shown when Eldev cannot be initialized.")

(defvar flymake-eldev--required-eldev-version "0.5")


(defun flymake-eldev-find-root (&optional from)
  "Get Eldev project root or nil, if not inside one.
If FROM is nil, search from `default-directory'."
  (when flymake-eldev-active
    (when-let (root (locate-dominating-file
                     (or from default-directory)
                     (lambda (dir) (or (file-exists-p (expand-file-name "Eldev" dir))
                                       (file-exists-p (expand-file-name "Eldev-local" dir))))))
      (expand-file-name root))))


;; Copied over from Flycheck.  See documentation and comments there.
(defun flymake-eldev--sexp-to-string (sexp)
  (let ((print-quoted t)
        (print-length nil)
        (print-level  nil))
    (prin1-to-string sexp)))

;; Copied over from Eldev.  See documentation and comments there.
(defmacro flymake-eldev--advised (spec &rest body)
  (declare (indent 1) (debug (sexp body)))
  (let ((symbol   (nth 0 spec))
        (where    (nth 1 spec))
        (function (nth 2 spec))
        (props    (nthcdr 3 spec))
        (fn       (make-symbol "$fn")))
    `(let ((,fn ,function))
       (when ,fn
         (if (advice-member-p ,fn ,symbol)
             (setf ,fn nil)
           (advice-add ,symbol ,where ,fn ,@props)))
       (unwind-protect
           ,(macroexp-progn body)
         (when ,fn
           (advice-remove ,symbol ,fn))))))


(defun flymake-eldev--byte-compile (original report-fn &rest args)
  (let* ((project-root        (flymake-eldev-find-root))
         (default-directory   (or project-root default-directory))
         (process-environment process-environment)
         (real-filename       (buffer-file-name)))
    (flymake-eldev--advised
        (#'make-process :around (when project-root
                                  (lambda (make-process &rest args)
                                    (let* ((command (plist-get args :command))
                                           (call    (cdr (--drop-while (not (string= it "-f")) command))))
                                      (if (= (length call) 2)
                                          (let ((output-buffer (plist-get args :buffer))
                                                (stderr-buffer (generate-new-buffer " *flymake-eldev stderr*"))
                                                (sentinel      (plist-get args :sentinel)))
                                            (plist-put args :command  (flymake-eldev--build-command-line real-filename (car call) (cadr call)))
                                            (plist-put args :stderr   stderr-buffer)
                                            (plist-put args :sentinel (lambda (eldev-process event)
                                                                        (let ((error-exit (and (not (process-live-p eldev-process))
                                                                                               (/= (process-exit-status eldev-process) 0))))
                                                                          (flymake-eldev--advised (#'process-exit-status :around
                                                                                                                         (when error-exit
                                                                                                                           (lambda (process-exit-status process)
                                                                                                                             (if (eq process eldev-process)
                                                                                                                                 0
                                                                                                                               (funcall process-exit-status process)))))
                                                                            (when error-exit
                                                                              (with-current-buffer output-buffer
                                                                                (erase-buffer)
                                                                                (insert (format ":elisp-flymake-output-start\n%S" `((,(flymake-eldev--extract-stderr stderr-buffer)
                                                                                                                                     1 nil :error))))))
                                                                            (when (buffer-live-p stderr-buffer)
                                                                              (kill-buffer stderr-buffer))
                                                                            (funcall sentinel eldev-process event))))))
                                        (warn "flymake-eldev: cannot build compilation command basing on %S" command)))
                                    (apply make-process args))))
      (when project-root
        (push (format "ELDEV_EMACS=%s" invocation-name) process-environment))
      (apply original report-fn args))))

(defun flymake-eldev--extract-stderr (buffer)
  (if (flymake-eldev--eldev-new-enough-p)
      (with-current-buffer buffer
        (let ((message (string-trim (buffer-string))))
          ;; Don't add clarification to a few obvious errors.
          (unless (or (string-match-p (rx bos "Dependency " (1+ any) " is not available") message))
            (setf message (concat message "\n\n" flymake-eldev-general-error)))
          message))
    (format-message "Eldev %s is required; please run `eldev upgrade-self'" flymake-eldev--required-eldev-version)))

(defun flymake-eldev--checkdoc (original &rest args)
  (apply original args))

(defun flymake-eldev--build-command-line (real-filename function temp-file)
  `("eldev"
    "--quiet" "--no-time" "--color=never" "--no-debug" "--no-backtrace-on-abort"
    "--as-is" "--load-newer"
    ,@(cond ((file-equal-p real-filename "Eldev")
             `("--setup-first" ,(flymake-eldev--sexp-to-string '(setf eldev-skip-project-config t))))
            ((file-equal-p real-filename "Eldev-local")
             `("--setup-first" ,(flymake-eldev--sexp-to-string '(setf eldev-skip-local-project-config t)))))
    ;; Replace the original file for project initialization purposes with the temporary.
    ;; If `eldev-project-main-file' is specified, this does nothing, but see the next bit
    ;; of setup.
    "--setup-first"
    ,(flymake-eldev--sexp-to-string
      `(advice-add #'eldev--package-dir-info :around
                   (lambda (original)
                     (eldev-advised
                         (#'insert-file-contents
                          :around (lambda (original filename &rest args)
                                    (apply original (if (file-equal-p filename ,real-filename) ,temp-file filename) args)))
                       (funcall original)))))
    ;; When checking project's main file, use the temporary as the main file instead.
    "--setup"
    ,(flymake-eldev--sexp-to-string
      `(when (and eldev-project-main-file (file-equal-p eldev-project-main-file ,real-filename))
         (setf eldev-project-main-file ,temp-file)))
    ;; Special handling for test files: load extra dependencies as if testing
    ;; now.  Likewise for loading roots.
    "--setup"
    ,(flymake-eldev--sexp-to-string
      `(when (eldev-filter-files '(,real-filename) eldev-test-fileset)
         (apply #'eldev-add-extra-dependencies 'exec (cdr (assq 'test eldev--extra-dependencies)))
         (apply #'eldev-add-loading-roots 'exec (cdr (assq 'test eldev--loading-roots)))))
    "exec" "--load" "--dont-require" "--lexical"
    ,(format "(%s %S)" function temp-file)))

(defun flymake-eldev--eldev-new-enough-p ()
  ;; Might want to cache at some point.  On the other hand, it's not clear how to
  ;; invalidate the cache to avoid false errors when Eldev is upgraded.
  (ignore-errors
    (with-temp-buffer
      (and (= (call-process "eldev" nil t nil "--quiet" "--setup-first" (flymake-eldev--sexp-to-string `(setf eldev-skip-project-config t)) "version") 0)
           (version<= flymake-eldev--required-eldev-version (string-trim (buffer-string)))))))


;;;###autoload
(defun flymake-eldev--initialize ()
  (advice-add #'elisp-flymake-byte-compile :around 'flymake-eldev--byte-compile)
  (advice-add #'elisp-flymake-checkdoc     :around 'flymake-eldev--checkdoc)
  ;; I don't think we need a separate package just for this, so let's do it here.
  (add-to-list 'auto-mode-alist `(,(rx "/" (or "Eldev" "Eldev-local") eos) . emacs-lisp-mode) t))


;;;###autoload
(eval-after-load 'flymake '(flymake-eldev--initialize))


(provide 'flymake-eldev)

;;; flymake-eldev.el ends here
