##
# Clang Makefile
##

# Project info
Project                := clang

# LLVM/Clang default configuration.

# Select default assertion mode, valid values are '0' and '1'.
Clang_Use_Assertions   := 1
# Select clang driver mode, valid values are 'Production' and 'Development'.
Clang_Driver_Mode      := Production
# Select optimization flags to use.
Clang_Optimize_Option  := -O2
# Additional arbitrary compiler options.
Clang_Extra_Options    := -g -DDISABLE_SMART_POINTERS

##

# Selection assertions mode.
ifeq ($(Clang_Use_Assertions), 1)
Assertions_Configure_Flag :=  --enable-assertions
else ifeq ($(Clang_Use_Assertions), 0)
Assertions_Configure_Flag :=  --disable-assertions
else
$(error "invalid setting for clang assertions: '$(Clang_Use_Assertions)'")
endif

# Set makefile variables to pass during build and install.
Clang_Make_Variables := KEEP_SYMBOLS=1 CLANG_VENDOR=Apple
ifeq ($(Clang_Driver_Mode), Production)
Clang_Make_Variables += CLANG_IS_PRODUCTION=1
else ifeq ($(Clang_Driver_Mode), Development)
# ... this is the default ...
else
$(error "invalid setting for clang driver mode: '$(Clang_Driver_Mode)'")
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
Extra_Configure_Flags = --enable-targets=x86,arm \
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

Install_Target = $(Clang_Make_Variables) install-clang
Build_Target = $(Clang_Make_Variables) clang-only

# Set default target.

all: install

###
# Utility targets for managing the integration branch.

SVN_BASE = $(shell svn info | sed -n 's/^URL: //; s,/llvm-project/.*$$,/llvm-project,p')
SVN_CLANG = $(shell svn info | sed -n 's/^URL: //p')
SVN_TAGS = $(SVN_BASE)/cfe/tags/Apple

update-clang-branch:
	if [ -n "$(LLVM_VERSION)" ]; then \
	  svn rm -m 'Update.' $(SVN_CLANG)/clang; \
	  svn cp -m 'Update.' $(SVN_BASE)/llvm/tags/Apple/llvmCore-$(LLVM_VERSION) $(SVN_CLANG)/clang; \
	  svn cp -m 'Update.' $(SVN_BASE)/cfe/branches/Apple/Dib $(SVN_CLANG)/clang/tools/clang; \
	  svn cp -m 'Update.' $(SVN_BASE)/compiler-rt/branches/Apple/Dib $(SVN_CLANG)/clang/projects/compiler-rt; \
	else \
	  echo "Usage: make LLVM_VERSION=2108 update-clang"; \
	fi

update-clang:
	svn rm -m 'Update.' $(SVN_CLANG)/clang
	svn cp -m 'Update.' $(SVN_BASE)/llvm/trunk $(SVN_CLANG)/clang
	svn cp -m 'Update.' $(SVN_BASE)/cfe/trunk $(SVN_CLANG)/clang/tools/clang
	svn cp -m 'Update.' $(SVN_BASE)/compiler-rt/trunk $(SVN_CLANG)/clang/projects/compiler-rt

update-to-tot:
	rm -rf clang
	svn co $(SVN_BASE)/llvm/trunk clang
	svn co $(SVN_BASE)/cfe/trunk clang/tools/clang

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
# Check that expected RC variables are set and sane.

ifneq (OK, $(shell test -d $(SRCROOT) && echo "OK"))
  $(error "invalid SRCROOT: '$(SRCROOT)'")
endif

ifeq ($(RC_ARCHS),)
  $(error "RC_ARCHS must be set")
endif
ifeq ($(OBJROOT),)
  $(error "OBJROOT must be set")
endif
ifeq ($(SYMROOT),)
  $(error "SYMROOT must be set")
endif
ifeq ($(DSTROOT),)
  $(error "DSTROOT must be set")
endif

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
Environment	= CC="$(CC) -arch $$arch" CXX="$(CXX) -arch $$arch" ONLY_MAN_DOCS=1
Configure_Flags = --prefix="$(Install_Prefix)" $(Extra_Configure_Flags)
Install_Flags	= DESTDIR=$(OBJROOT)/install-$$arch

OSV		= $(DSTROOT)/usr/local/OpenSourceVersions
OSL		= $(DSTROOT)/usr/local/OpenSourceLicenses

##
# Build Logic

.PHONY: install installsrc installhdrs build configure lazy_install_source install_source clean

SYSCTL := $(shell if [ `sysctl -n hw.activecpu` -ge 8 -a `sysctl -n hw.memsize` -le 2147483648 ]; then echo 4; else sysctl -n hw.activecpu; fi)

installsrc: install_source

installhdrs:

install:: build
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Installing $(Project) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/install-$$arch && \
		umask 0022 && \
		$(MAKE) -C $(OBJROOT)/$$arch $(Environment) \
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

build:: configure
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Building $(Project) for $$arch..." && \
		$(MAKE) -j$(SYSCTL) -C $(OBJROOT)/$$arch $(Environment) $(Build_Target) || exit 1; \
	done

configure:: lazy_install_source
	$(_v) $(MKDIR) $(OBJROOT)
	$(_v) for arch in $(RC_ARCHS) ; do \
		echo "Configuring $(Project) for $$arch..." && \
		$(MKDIR) $(OBJROOT)/$$arch && \
		cd $(OBJROOT)/$$arch && \
		$(Environment) $(Configure) $(Configure_Flags) || exit 1 ; \
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
	if [ -f ../../$(Install_Prefix)/bin/clang++ ]; then \
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
