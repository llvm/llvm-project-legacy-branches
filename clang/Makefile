##
# Clang Makefile
##

# Project info
Project                := clang

# LLVM/Clang default configuration.

# Select default assertion mode, valid values are '0' and '1'.
Clang_Use_Assertions   := 0
# Select clang driver mode, valid values are 'Production' and 'Development'.
Clang_Driver_Mode      := Production
# Select optimization flags to use.
Clang_Optimize_Option  := -O2
# Additional arbitrary compiler options.
Clang_Extra_Options    := -g -DDISABLE_SMART_POINTERS
# Enable use of clang++?
Clang_Enable_CXX       := 0
# Enable bootstrap build.
Clang_Enable_Bootstrap := 0
# Build all LLVM tools; not just clang?
Clang_Build_All        := 0

##

# Selection assertions mode.
ifeq ($(Clang_Use_Assertions), 1)
Assertions_Configure_Flag :=  --enable-assertions
else ifeq ($(Clang_Use_Assertions), 0)
Assertions_Configure_Flag :=  --disable-assertions
else
$(error "invalid setting for clang assertions: '$(Clang_Use_Assertions)'")
endif

# Select whether to build everything (for testing purposes).
ifeq ($(Clang_Build_All), 1)
Clang_Build_Target := all
else ifeq ($(Clang_Build_All), 0)
Clang_Build_Target := clang-only
else
$(error "invalid setting for clang build all mode: '$(Clang_Build_All)'")
endif

# Set makefile variables to pass during build and install.
Clang_Make_Variables := KEEP_SYMBOLS=1 CLANG_VENDOR=Apple
ifeq ($(Clang_Driver_Mode), Production)
Clang_Make_Variables += CLANG_IS_PRODUCTION=1

ifeq ($(Clang_Enable_CXX), 1)
Clang_Make_Variables += CLANGXX_IS_PRODUCTION=1
else ifeq ($(Clang_Enable_CXX), 0)
# ... this is the default ...
else
$(error "invalid setting for clang enable C++: '$(Clang_Enable_CXX)'")
endif

else ifeq ($(Clang_Driver_Mode), Development)
# ... this is the default ...
else
$(error "invalid setting for clang driver mode: '$(Clang_Driver_Mode)'")
endif

# Additional build logic when building as a cross compiler.
ifeq ($(MAKECMDGOALS),install-cross)
INSTALL_LOCATION := /
endif

# Set destination information.
ifneq ($(INSTALL_LOCATION),)
Install_Root := $(INSTALL_LOCATION)
Post_Install_RootLinks := 0
Post_Install_OpenSourceLicense := 0
else
Install_Root := /Developer
Post_Install_RootLinks := 1
Post_Install_OpenSourceLicense := 1
endif

# Set Install_Prefix and validate it exists.
Install_Prefix := $(Install_Root)/usr
ifneq (OK, $(shell test -d $(Install_Prefix) && echo "OK"))
  $(error "invalid install prefix: '$(Install_Prefix)'")
endif

# Set configure flags.
Configure_Flags = --enable-targets=x86,arm \
		  --enable-optimized \
		  $(Assertions_Configure_Flag) \
                  --with-optimize-option="$(Clang_Optimize_Option)" \
                  --with-extra-options="$(Clang_Extra_Options)" \
		  --disable-doxygen

# Set up post install targets.
Post_Install_Targets :=

# Install /usr/... symlinks?
ifeq ($(Post_Install_RootLinks),1)
Post_Install_Targets += post-install-rootlinks
else ifneq ($(Post_Install_RootLinks),0)
$(error "unknown value for post install of root symlinks: '$(Post_Install_RootLinks)'")
endif

# Install open source license?
ifeq ($(Post_Install_OpenSourceLicense),1)
Post_Install_Targets += post-install-opensourcelicense
else ifneq ($(Post_Install_OpenSourceLicense),0)
$(error "unknown value for post install of open source license: '$(Post_Install_OpenSourceLicense)'")
endif

# Select final configure target.
ifeq ($(Clang_Enable_Bootstrap), 1)
Final_Configure_Target := configure_stage2
else ifeq ($(Clang_Enable_Bootstrap), 0)
Final_Configure_Target := configure_singlestage
else
$(error "invalid setting for clang enable bootstrap: '$(Clang_Enable_Bootstrap)'")
endif

# Set install and build targets.
Install_Target = $(Clang_Make_Variables) install-clang
Build_Target = $(Clang_Make_Variables) $(Clang_Build_Target)

# Set default target.

all: install

###
# Utility targets for managing the integration branch.

SVN_BASE = $(shell svn info | sed -n 's/^URL: //; s,/llvm-project/.*$$,/llvm-project,p')
SVN_CLANG = $(shell svn info | sed -n 's/^URL: //p')
SVN_TAGS = $(SVN_BASE)/cfe/tags/Apple

update-clang:
	svn rm -m 'Update.' $(SVN_CLANG)/clang
	svn cp -m 'Update.' $(SVN_BASE)/llvm/branches/Apple/williamson $(SVN_CLANG)/clang
	svn cp -m 'Update.' $(SVN_BASE)/cfe/branches/Apple/williamson $(SVN_CLANG)/clang/tools/clang
	svn cp -m 'Update.' $(SVN_BASE)/compiler-rt/trunk $(SVN_CLANG)/clang/projects/compiler-rt

tag-clang:
	if [ -n "$(VERSION)" ]; then \
	  svn cp -m 'Tag.' $(SVN_CLANG) $(SVN_TAGS)/clang-$(VERSION); \
	else \
	  echo Usage: make VERSION=25 tag-clang; \
	fi

retag-clang:
	if [ -n "$(VERSION)" ]; then \
	  svn rm -m 'Retag.' $(SVN_TAGS)/clang-$(VERSION) && \
	  svn cp -m 'Retag.' $(SVN_CLANG) $(SVN_TAGS)/clang-$(VERSION); \
	else \
	  echo Usage: make VERSION=25 retag-clang; \
	fi

##
# Additional Tool Paths

CHOWN		= /usr/sbin/chown
CXX             = /usr/bin/g++
FIND		= /usr/bin/find
INSTALL		= /usr/bin/install
INSTALL_FILE	= $(INSTALL) -m 0444
MKDIR		= /bin/mkdir -p -m 0755
PAX		= /bin/pax
RMDIR		= /bin/rm -fr
XARGS		= /usr/bin/xargs

## 
# Tool Variables

Cruft      = CVS RCS SCCS *~ .*~ .nfs\* .*.wmd .svn .DS_Store
Find_Cruft = '(' $(Cruft:%=-name '%' -or) -name '' ')' -print

##
# Assorted variables

Sources		= $(SRCROOT)/$(Project)
Configure	= $(Sources)/configure
Install_Flags	= DESTDIR=$(OBJROOT)/install-$$arch ONLY_MAN_DOCS=1

OSV		= $(DSTROOT)/usr/local/OpenSourceVersions
OSL		= $(DSTROOT)/usr/local/OpenSourceLicenses

##
# Cross-builds need wrapper scripts on the path, so have a local directory
# available for them.

PATH := ${OBJROOT}/bin:${PATH}

##
# Build Logic

.PHONY: install installsrc installhdrs \
	build build_final build_stage1 \
	configure_final configure_singlestage configure_stage2 configure_stage1 \
	lazy_install_source install_source clean

SYSCTL := $(shell if [ `sysctl -n hw.activecpu` -ge 8 -a `sysctl -n hw.memsize` -le 2147483648 ]; then echo 4; else sysctl -n hw.activecpu; fi)

installsrc: install_source

installhdrs:

##
# Cross Compilation Build Support

install-cross:: build-cross
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Installing $(Project) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/install-$$arch && \
		umask 0022 && \
		$(MAKE) -C $(OBJROOT)/$$arch \
		  $(Install_Flags) $(Install_Target) CLANG_NO_RUNTIME=1 || exit 1 ; \
	done
	$(_v) rm -rf $(DSTROOT)
	$(_v) mkdir -p $(DSTROOT)
	$(_v) for arch in $(filter-out $(firstword $(RC_ARCHS)),$(RC_ARCHS)); do \
		rm -rf $(OBJROOT)/install-$$arch$(Install_Prefix)/lib/clang/*/lib; \
	done
	./merge-lipo `for arch in $(RC_ARCHS) ; do echo $(OBJROOT)/install-$$arch ; done` $(DSTROOT)
	$(_v) $(FIND) $(DSTROOT) $(Find_Cruft) | $(XARGS) $(RMDIR)
	$(_v) $(FIND) $(SYMROOT) $(Find_Cruft) | $(XARGS) $(RMDIR)
	$(_v) $(FIND) $(DSTROOT) -perm -0111 ! -name ccc -type f -print | $(XARGS) -n 1 -P $(SYSCTL) dsymutil
	$(_v) cd $(DSTROOT) && find . -path \*.dSYM/\* -print | cpio -pdml $(SYMROOT)
	$(_v) find $(DSTROOT) -perm -0111 ! -name ccc -type f -print | xargs -P $(SYSCTL) strip
	$(_v) find $(DSTROOT) -name \*.dSYM -print | xargs rm -r
	$(_v)- $(CHOWN) -R root:wheel $(DSTROOT) $(SYMROOT)

build-cross:: configure-cross
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Building (Cross) $(Project) for $$arch..." && \
		$(MAKE) -j$(SYSCTL) -C $(OBJROOT)/$$arch $(Build_Target) CFLAGS="-arch $$arch $(CFLAGS)" CXXFLAGS="-arch $$arch $(CXXFLAGS)" OPTIONAL_DIRS= CLANG_NO_RUNTIME=1 || exit 1; \
	done

configure-cross:: setup-tools-cross
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Configuring (Cross) $(Project) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/$$arch && \
		cd $(OBJROOT)/$$arch && \
		$(Configure) --prefix="$(Install_Prefix)" $(Configure_Flags) \
			--host=arm-apple-darwin10 \
			--target=arm-apple-darwin10 \
			--build=i686-apple-darwin10 \
		  || exit 1 ; \
	done

# A cross-compiler configure will expect to find tools under names like
# arm-apple-darwin10-gcc, so make sure we have them. Note that -marm
# is added to the gcc/g++ command line due to rdar://7353031
setup-tools-cross::
	$(_v) $(MKDIR) $(OBJROOT)/bin
	$(_v) for prog in ar nm ranlib strip lipo ld as ; do \
	  echo '#!/bin/sh' > $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  echo 'exec '`xcrun -sdk $(SDKROOT) -find $$prog` '"$$@"' \
	    >> $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  chmod a+x $(OBJROOT)/bin/arm-apple-darwin10-$$prog || exit 1; \
	done
	$(_v) for prog in gcc g++ ; do \
	  echo '#!/bin/sh' > $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  echo "ARCH='-arch armv6'" >> $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  echo 'for i in $$@ ; do if [ "$$i" == "-arch" ] ; then ARCH= ; fi ; done' >> $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  echo 'exec '`xcrun -find $$prog` \
	    ' $$ARCH -isysroot '$(SDKROOT)' "$$@" -marm' \
	    >> $(OBJROOT)/bin/arm-apple-darwin10-$$prog && \
	  chmod a+x $(OBJROOT)/bin/arm-apple-darwin10-$$prog || exit 1 ; \
	done

##
# Standard Build Support

install:: build
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Installing $(Project) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/install-$$arch && \
		umask 0022 && \
		$(MAKE) -C $(OBJROOT)/$$arch \
		  $(Install_Flags) $(Install_Target) || exit 1 ; \
	done
	$(_v) rm -rf $(DSTROOT)
	$(_v) mkdir -p $(DSTROOT)
	$(_v) for arch in $(filter-out $(firstword $(RC_ARCHS)),$(RC_ARCHS)); do \
		rm -rf $(OBJROOT)/install-$$arch$(Install_Prefix)/lib/clang/*/lib; \
	done
	./merge-lipo `for arch in $(RC_ARCHS) ; do echo $(OBJROOT)/install-$$arch ; done` $(DSTROOT)
	$(_v) $(FIND) $(DSTROOT) $(Find_Cruft) | $(XARGS) $(RMDIR)
	$(_v) $(FIND) $(SYMROOT) $(Find_Cruft) | $(XARGS) $(RMDIR)
	$(_v) $(FIND) $(DSTROOT) -perm -0111 ! -name ccc -type f -print | $(XARGS) -n 1 -P $(SYSCTL) dsymutil
	$(_v) cd $(DSTROOT) && find . -path \*.dSYM/\* -print | cpio -pdml $(SYMROOT)
	$(_v) find $(DSTROOT) -perm -0111 ! -name ccc -type f -print | xargs -P $(SYSCTL) strip
	$(_v) find $(DSTROOT) -name \*.dSYM -print | xargs rm -r
	$(_v)- $(CHOWN) -R root:wheel $(DSTROOT) $(SYMROOT)
	$(_v) $(MAKE) post-install

build:: build_final

build_final:: configure_final
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Building (Final) $(Project) for $$arch..." && \
		$(MAKE) -j$(SYSCTL) -C $(OBJROOT)/$$arch $(Build_Target) || exit 1; \
	done

build_stage1:: configure_stage1
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Building (Stage 1) $(Project) for $$arch..." && \
		$(MAKE) -j$(SYSCTL) -C $(OBJROOT)/stage1-$$arch $(Build_Target) || exit 1; \
		$(MAKE) -j$(SYSCTL) -C $(OBJROOT)/stage1-$$arch $(Install_Target) || exit 1; \
	done

configure_final:: $(Final_Configure_Target)

configure_stage2:: build_stage1
	$(_v) $(MKDIR) $(OBJROOT)
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Configuring (Final) $(Project) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/$$arch && \
		cd $(OBJROOT)/$$arch && \
		$(Configure) --prefix="$(Install_Prefix)" $(Configure_Flags) \
		  CC="$(OBJROOT)/stage1-install-$$arch/bin/clang -arch $$arch" \
		  CXX="$(OBJROOT)/stage1-install-$$arch/bin/clang++ -arch $$arch" || exit 1 ; \
	done

configure_singlestage::
	$(_v) $(MKDIR) $(OBJROOT)
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Configuring (Final) $(Project) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/$$arch && \
		cd $(OBJROOT)/$$arch && \
		$(Configure) --prefix="$(Install_Prefix)" $(Configure_Flags) \
		  CC="$(CC) -arch $$arch" \
		  CXX="$(CXX) -arch $$arch" || exit 1 ; \
	done

configure_stage1:: 
	$(_v) $(MKDIR) $(OBJROOT)
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Configuring (Stage 1) $(Project) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/stage1-$$arch && \
		cd $(OBJROOT)/stage1-$$arch && \
		$(Configure) --prefix="$(OBJROOT)/stage1-install-$$arch" $(Configure_Flags) \
		  CC="$(CC) -arch $$arch" CXX="$(CXX) -arch $$arch" || exit 1 ; \
	done

lazy_install_source::
	$(_v) if [ ! -f "$(SRCROOT)/Makefile" ]; then $(MAKE) install_source; fi

install_source::
	@echo "Installing source for $(Project)..."
	$(_v) $(MKDIR) "$(SRCROOT)"
	$(_v) $(PAX) -rw . "$(SRCROOT)"
	$(_v) $(FIND) "$(SRCROOT)" $(Find_Cruft) -depth -exec $(RMDIR) "{}" \;

clean::
	if [ "$(SRCROOT)" != . ]; then \
	  rm -rf "$(SRCROOT)/clang/test/Archive"; \
	fi

post-install: $(Post_Install_Targets)

post-install-rootlinks:
	$(MKDIR) -p $(DSTROOT)/usr/bin
	ln -sf ../../$(Install_Prefix)/bin/clang $(DSTROOT)/usr/bin/clang
	if [ -f $(DSTROOT)/$(Install_Prefix)/bin/clang++ ]; then \
	  ln -sf ../../$(Install_Prefix)/bin/clang++ $(DSTROOT)/usr/bin/clang++; \
	fi
	$(MKDIR) -p $(DSTROOT)/usr/share/man/man1/
	cp $(DSTROOT)/$(Install_Prefix)/share/man/man1/clang.1 $(DSTROOT)/usr/share/man/man1/

post-install-opensourcelicense:
	$(MKDIR) $(OSV)
	$(INSTALL_FILE) $(SRCROOT)/$(Project).plist $(OSV)/$(Project).plist
	$(MKDIR) $(OSL)
	$(INSTALL_FILE) $(Sources)/LICENSE.TXT $(OSL)/$(Project)-llvm.txt
	$(INSTALL_FILE) $(Sources)/tools/clang/LICENSE.TXT $(OSL)/$(Project).txt
