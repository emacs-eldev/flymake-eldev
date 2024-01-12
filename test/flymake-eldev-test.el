;; -*- lexical-binding: t -*-

(require 'flymake-eldev)
(require 'dash)
(require 'ert)


(defvar flymake-eldev--test-dir (file-name-directory (or load-file-name (buffer-file-name))))


(defmacro flymake-eldev--test (file &rest body)
  (declare (indent 1) (debug (sexp body)))
  `(let ((file                         (expand-file-name ,file flymake-eldev--test-dir))
         ;; Not needed for our tests, only results in an ugly warning (at least on Emacs
         ;; 29).  Force-disable it.
         (flymake-diagnostic-functions (delq 'flymake-proc-legacy-flymake flymake-diagnostic-functions)))
     (with-temp-buffer
       (insert-file-contents file t)
       (setf default-directory (file-name-directory file))
       (emacs-lisp-mode)
       (flymake-mode 1)
       ,@body)))

(defmacro flymake-eldev--test-with-temp-file (file content-creation &rest body)
  (declare (indent 2) (debug (sexp form body)))
  `(let ((file (expand-file-name ,file flymake-eldev--test-dir)))
     (ignore-errors (delete-file file))
     (with-temp-file file
       ,content-creation)
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-file file)))))

;; FIXME: Force checkdoc test too.  I have no idea how to do it, `flymake-start' is apparently not enough.
(defun flymake-eldev--test-recheck ()
  (flymake-start)
  ;; As always, we have to dig through internals.
  (should-not (flymake--state-reported-p (gethash 'elisp-flymake-byte-compile flymake--state)))
  (let ((start-time (float-time)))
    (while (not (flymake--state-reported-p (gethash 'elisp-flymake-byte-compile flymake--state)))
      ;; Be generous, what if Eldev needs to install dependencies?
      (when (> (- (float-time) start-time) 3.0) ; DONOTCOMMIT: was 30.0
        (ert-fail "timed out"))
      (accept-process-output nil 0.02))
    (should (flymake--state-reported-p (gethash 'elisp-flymake-byte-compile flymake--state)))))

(defun flymake-eldev--test-expect-no-errors ()
  ;; Only exists for a better name.
  (flymake-eldev--test-expect-errors))

(defun flymake-eldev--test-expect-errors (&rest errors)
  ;(eldev-dump (flymake--state-diags (gethash 'elisp-flymake-byte-compile flymake--state)))
  ;; (eldev-dump (flymake--state-foreign-diags (gethash 'elisp-flymake-byte-compile flymake--state)))
  ;; (eldev-dump (flymake-diagnostics))
  ;; (with-current-buffer " *stderr of elisp-flymake-byte-compile*"
  ;;   (eldev-dump (buffer-string)))
  (let ((actual-errors   (--sort (< (flymake--diag-beg it) (flymake--diag-beg other))
                                 (flymake--state-diags (gethash 'elisp-flymake-byte-compile flymake--state))))
        (expected-errors errors))
    (while (or actual-errors expected-errors)
      (let ((actual   (pop actual-errors))
            (expected (pop expected-errors)))
        ;(eldev-dump actual expected errors)
        (cond ((and actual expected)
               (when-let (regexp (plist-get expected :matches))
                 (unless (string-match-p regexp (flymake--diag-text actual))
                   (ert-fail (format "unexpected error %S: expected message matching '%s'" actual regexp)))))
              (actual
               (ert-fail (format "unexpected error: %S" actual)))
              (expected
               (ert-fail (format "expected error not detected: %S" expected))))))))


(ert-deftest flymake-eldev-basics-1 ()
  (flymake-eldev--test "project-a/project-a.el"
    (flymake-eldev--test-recheck)
    (flymake-eldev--test-expect-no-errors)))

(ert-deftest flymake-eldev-basics-2 ()
  (flymake-eldev--test "project-a/Eldev"
    ;; Enable `checkdoc'.  Must be disabled at runtime (currently through our own hack)
    ;; for the test to pass.
    :enable-checkdoc t
    (flymake-eldev--test-recheck)
    (flymake-eldev--test-expect-no-errors)))

(ert-deftest flymake-eldev-basics-3 ()
  (flymake-eldev--test "project-a/project-a-suspicious-stuff.el"
    (flymake-eldev--test-recheck)
    (flymake-eldev--test-expect-errors '(:matches "this-function-has-never-been-declared")
                                       '(:matches "this-variable-has-never-been-declared"))))

(ert-deftest flymake-eldev-self-1 ()
  (flymake-eldev--test "../flymake-eldev.el"
    (flymake-eldev--test-recheck)
    (flymake-eldev--test-expect-no-errors)))

(ert-deftest flymake-eldev-self-2 ()
  (flymake-eldev--test "flymake-eldev-test.el"
    (flymake-eldev--test-recheck)
    (flymake-eldev--test-expect-no-errors)))

;; This indirectly tests that we see functions in `eldev' namespace from `Eldev' and
;; `Eldev-local'.  Unfortunately, we also see them in normal files, but I cannot think of
;; a robust way to avoid this currently.
(ert-deftest flymake-eldev-self-3 ()
  (flymake-eldev--test "../Eldev"
    ;; Enable `checkdoc'.  Must be disabled at runtime (currently through our own hack)
    ;; for the test to pass.
    :enable-checkdoc t
    (flymake-eldev--test-recheck)
    (flymake-eldev--test-expect-no-errors)))

(ert-deftest flymake-eldev-test-file-1 ()
  (flymake-eldev--test "project-a/test/project-a.el"
    (flymake-eldev--test-recheck)
    (flymake-eldev--test-expect-no-errors)))

(ert-deftest flymake-eldev-deactivating-1 ()
  (let ((flymake-eldev-active nil))
    (flymake-eldev--test "project-a/project-a.el"
      (flymake-eldev--test-recheck)
      (flymake-eldev--test-expect-errors '(:matches "dependency-a")))))

;; Test that removing dependencies gives errors immediately.
(ert-deftest flymake-eldev-remove-dependency-1 ()
  (flymake-eldev--test "project-a/project-a.el"
    (search-forward "(dependency-a \"1.0\")")
    (replace-match "")
    (flymake-eldev--test-recheck)
    (flymake-eldev--test-expect-errors '(:matches "dependency-a"))))

;; Test that adding unaccessible dependencies gives errors immediately.
(ert-deftest flymake-eldev-add-dependency-1 ()
  (flymake-eldev--test "project-a/project-a.el"
    (search-forward "(dependency-a \"1.0\")")
    (insert " (some-totally-bullshit-dependency)")
    (flymake-eldev--test-recheck)
    (flymake-eldev--test-expect-errors '(:matches "not available"))))

;; Test that faulty project initialization code is handled fine.
(ert-deftest flymake-eldev-faulty-eldev-local-1 ()
  (flymake-eldev--test-with-temp-file "project-a/Eldev-local"
      (insert ";;  -*- lexical-binding: t -*-\nthis-variable-certainly-doesnt-exist\n")
    (flymake-eldev--test "project-a/project-a.el"
      (flymake-eldev--test-recheck)
      (flymake-eldev--test-expect-errors '(:matches "cannot be initialized")))))

;; When checking the faulty `Eldev-local', we must not use it for initialization.
(ert-deftest flymake-eldev-faulty-eldev-local-2 ()
  (flymake-eldev--test-with-temp-file "project-a/Eldev-local"
      (insert ";;  -*- lexical-binding: t -*-\nthis-variable-certainly-doesnt-exist\n")
    (flymake-eldev--test "project-a/Eldev-local"
      (flymake-eldev--test-recheck)
      (flymake-eldev--test-expect-errors '(:matches "this-variable-certainly-doesnt-exist")))))

;; Test that `flymake-eldev' really works on Eldev files if those are byte-compilable.
(ert-deftest flymake-eldev-suspicious-eldev-local-1 ()
  (flymake-eldev--test-with-temp-file "project-a/Eldev-local"
      (insert ";;  -*- lexical-binding: t -*-\n(defun just-for-testing () (function-with-this-name-certainly-doesnt-exist))\n")
    (flymake-eldev--test "project-a/Eldev-local"
      (flymake-eldev--test-recheck)
      (flymake-eldev--test-expect-errors '(:matches "function-with-this-name-certainly-doesnt-exist")))))
