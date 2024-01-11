;; -*- lexical-binding: t -*-

;; Using loading root to test that `flymake-eldev' handles this too.
(require 'test-util)

(ert-deftest project-a-test-hello ()
  (should (string= (project-a-hello) "Hello")))
