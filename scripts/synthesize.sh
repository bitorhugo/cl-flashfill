#!/bin/sh

# loads QUICKLISP, then CL-FLASHFILL, and then calls Synthesize
#
sbcl --noinform --no-userinit --non-interactive \
  --eval '(let ((p (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file p) (load p)))' \
  --eval '(push (uiop:getcwd) asdf:*central-registry*)' \
  --eval '(ql:quickload :flashfill :silent t)' \
  --eval "(format t \"~&~a~%\" (flashfill::synthesize '$1))"
