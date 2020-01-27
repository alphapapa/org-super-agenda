# * makem.sh/Makefile --- Script to aid building and testing Emacs Lisp packages

# This Makefile is from the makem.sh repo: <https://github.com/alphapapa/makem.sh>.

# * Arguments

# For consistency, we use only var=val options, not hyphen-prefixed options.

# NOTE: I don't like duplicating the arguments here and in makem.sh,
# but I haven't been able to find a way to pass arguments which
# conflict with Make's own arguments through Make to the script.
# Using -- doesn't seem to do it.

ifdef auto-install
	AUTO_INSTALL = "--auto-install"
endif

ifdef sandbox
	SANDBOX = "--sandbox"
endif

ifdef sandbox-dir
	SANDBOX_DIR = "--sandbox-dir" "$(sandbox-dir)"
endif

ifdef debug
	DEBUG = "--debug"
endif

# ** Verbosity

# Since the "-v" in "make -v" gets intercepted by Make itself, we have
# to use a variable.

verbose = $(v)

ifneq (,$(findstring vv,$(verbose)))
	VERBOSE = "-vv"
else ifneq (,$(findstring v,$(verbose)))
	VERBOSE = "-v"
endif

# * Rules

# TODO: Handle cases in which "test" or "tests" are called and a
# directory by that name exists, which can confuse Make.

%:
	@./makem.sh $(DEBUG) $(VERBOSE) $(SANDBOX) $(SANDBOX_DIR) $(AUTO_INSTALL) $(@)
