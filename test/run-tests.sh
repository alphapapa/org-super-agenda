#!/bin/sh -e

# Copied from package-lint/run-tests.sh

EMACS="${EMACS:=emacs}"

INIT_PACKAGE_EL="(progn
  (require 'package)
  (push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)
  (package-initialize))"

PACKAGES_NEEDED="
(setq packages-needed '(ht f))
(cl-loop for package in packages-needed
         unless (package-installed-p package)
         do (package-install package))"

# Refresh package archives, because the test suite needs to see at least
# package-lint and cl-lib.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         --eval "(unless (package-installed-p 'cl-lib) (package-install 'cl-lib))" \
         --eval "$PACKAGES_NEEDED"

# Byte compile, failing on byte compiler errors, or on warnings unless ignored
if [ -n "${EMACS_LINT_IGNORE+x}" ]; then
    ERROR_ON_WARN=nil
else
    ERROR_ON_WARN=t
fi
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l org-super-agenda.el \
         --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
         -f batch-byte-compile \
         org-super-agenda.el test/test.el

# Lint ourselves
# Lint failures are ignored if EMACS_LINT_IGNORE is defined, so that lint
# failures on Emacs 24.2 and below don't cause the tests to fail, as these
# versions have buggy imenu that reports (defvar foo) as a definition of foo.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l org-super-agenda.el \
         -f package-lint-batch-and-exit \
         org-super-agenda.el test/test.el || [ -n "${EMACS_LINT_IGNORE+x}" ]

# Finally, run the testsuite
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l org-super-agenda.el \
         -l test/test.el \
         -f ert-run-tests-batch-and-exit
