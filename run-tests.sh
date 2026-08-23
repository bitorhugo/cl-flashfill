#!/bin/sh

# loads QUICKLISP, then CL-FLASHFILL, and then calls RUN-TESTS
#
sbcl --no-userinit --non-interactive \
  --eval '(let ((p (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file p) (load p)))' \
  --eval '(push (uiop:getcwd) asdf:*central-registry*)' \
  --eval '(ql:quickload :cl-flashfill :silent t)' \
  --eval '(cl-flashfill::run-tests)'
