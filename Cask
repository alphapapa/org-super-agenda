;; NOTE: We load a specific version of Org to prevent changes in Org from affecting the test
;; results.  This locked version will need to be updated manually from time to time.  Also, note
;; that locking the version here means that test results can differ between runs from Cask and runs
;; from an interactive Emacs instance.  For example, Org commits
;; b5c19643d2347e4e1f4d76634b458380a46e39c4 and 00f95cb126af6239cf0f18a4db21bd47614f6894 changed the
;; "Scheduled:" agenda repeater by 1 (i.e. "2x" became "1x"), which changed all of the test results
;; (see <https://lists.gnu.org/archive/html/emacs-orgmode/2017-03/msg00361.html>).

(source gnu)
(source melpa)
(source "org" "http://orgmode.org/elpa/")

(package-file "org-super-agenda.el")

(development
 ;; NOTE: We load org-plus-contrib because that's the only way to load a version of Org different
 ;; than the one included with Emacs.  See <https://github.com/cask/cask/issues/169>.
 (depends-on "org-plus-contrib" "9.1")
 (depends-on "f")
 (depends-on "ecukes")
 (depends-on "ert-runner")
 (depends-on "el-mock"))
