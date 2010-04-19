##
# Clang Makefile
##

# Project info
Project                := clang
UserType               := Developer
ToolType               := Commands

# LLVM/Clang default configuration.

# Select default assertion mode, valid values are '0' and '1'.
Clang_Use_Assertions   := 1
# Select clang driver mode, valid values are 'Production' and 'Development'.
Clang_Driver_Mode      := Production
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
Install_Prefix := $(Install_Root)/usr

# Set and validate Install_Prefix exists.
ifneq (OK, $(shell test -d $(Install_Prefix) && echo "OK"))
  $(error "invalid install prefix: '$(Install_Prefix)'")
endif

# Set configure flags.
Extra_Configure_Flags = --enable-targets=x86,arm \
			--enable-optimized \
			$(Assertions_Configure_Flag) \
			--with-extra-options='-g -DDISABLE_SMART_POINTERS' \
			--disable-doxygen \
			$(HOST_TARGET_FLAGS)

# Set up post install targets.
GnuAfterInstall      := post-install
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

# It's a GNU Source project
include ./GNUSource.make

CXX := /usr/bin/g++

# Automatic Extract & Patch
#AEP            = YES
#AEP_Project    = clang
#AEP_Version    = 090201
#AEP_ProjVers   = $(AEP_Project)-$(AEP_Version)
#AEP_Filename   = $(AEP_ProjVers).tar.bz2
#AEP_ExtractDir = $(AEP_ProjVers)
#AEP_Patches    = 

ifeq ($(suffix $(AEP_Filename)),.bz2)
AEP_ExtractOption = j
else
AEP_ExtractOption = z
endif

Install_Target = $(Clang_Make_Variables) install-clang
Build_Target = $(Clang_Make_Variables) $(Clang_Build_Target)

# Extract the source.
install_source::
ifeq ($(AEP),YES)
	$(TAR) -C $(SRCROOT) -$(AEP_ExtractOption)xf $(SRCROOT)/$(AEP_Filename)
	$(RMDIR) $(SRCROOT)/$(Project)
	$(MV) $(SRCROOT)/$(AEP_ExtractDir) $(SRCROOT)/$(Project)
	for patchfile in $(AEP_Patches); do \
		cd $(SRCROOT)/$(Project) && \
		patch -p0 < $(SRCROOT)/patches/$$patchfile || exit 1 ;  \
	done
endif

# Available post install targets.
post-install: $(Post_Install_Targets)

post-install-rootlinks:
	$(MKDIR) -p $(DSTROOT)/usr/bin
	ln -sf ../../$(Install_Prefix)/bin/clang $(DSTROOT)/usr/bin/clang
	if [ -f ../../$(Install_Prefix)/bin/clang++ ]; then \
	  ln -sf ../../$(Install_Prefix)/bin/clang++ $(DSTROOT)/usr/bin/clang++; \
	fi
	$(MKDIR) -p $(DSTROOT)/usr/share/man/man1/
	cp $(DSTROOT)/$(Install_Prefix)/share/man/man1/clang.1 $(DSTROOT)/usr/share/man/man1/

OSV = $(DSTROOT)/usr/local/OpenSourceVersions
OSL = $(DSTROOT)/usr/local/OpenSourceLicenses

post-install-opensourcelicense:
	$(MKDIR) $(OSV)
	$(INSTALL_FILE) $(SRCROOT)/$(Project).plist $(OSV)/$(Project).plist
	$(MKDIR) $(OSL)
	$(INSTALL_FILE) $(Sources)/LICENSE.TXT $(OSL)/$(Project)-llvm.txt
	$(INSTALL_FILE) $(Sources)/tools/clang/LICENSE.TXT $(OSL)/$(Project).txt

SVN_BASE = $(shell svn info | sed -n 's/^URL: //; s,/llvm-project/.*$$,/llvm-project,p')
SVN_CLANG = $(shell svn info | sed -n 's/^URL: //p')
SVN_TAGS = $(SVN_BASE)/cfe/tags/Apple

# FIXME: Switch to LLVM tag ASAP.
update-clang:
	svn rm -m 'Update.' $(SVN_CLANG)/clang
	svn cp -m 'Update.' $(SVN_BASE)/llvm/branches/Apple/Morbo@101744 $(SVN_CLANG)/clang
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
