##
# Clang Makefile
##

# Project info
Project                := objc_translate

##

SVN_BASE = $(shell svn info | sed -n "s/^URL: //; s,/llvm-project/.*$$,/llvm-project,p")
SVN_CLANG = $(shell svn info | sed -n "s/^URL: //p")

all:
	@echo "usage: make [target]"
	@echo ""
	@echo "Available Targets:"
	@echo "  update-sources: update LLVM and Clang sources"

update-sources:
	echo svn rm -m 'Update.' $(SVN_CLANG)/src
	echo svn cp -m 'Update.' $(SVN_BASE)/llvm/trunk $(SVN_CLANG)/src
	echo svn cp -m 'Update.' $(SVN_BASE)/cfe/trunk $(SVN_CLANG)/src/tools/clang

###
# Debugging

make-print-%:
	$(error PRINT: $(value $*) = "$($*)" (from $(origin $*)))
