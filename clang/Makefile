##
# cxxfilt Makefile
##

# Project info
Project               = clang
UserType              = Developer
ToolType              = Commands
Install_Prefix	      = /Developer/usr
Extra_Configure_Flags = --enable-targets=powerpc,x86,arm,cbe,cpp \
			--enable-optimized --disable-doxygen \
			$(HOST_TARGET_FLAGS)
Extra_CC_Flags        = -mdynamic-no-pic
GnuAfterInstall       = post-install install-plist
CXX = g++
CXX = g++

# It's a GNU Source project
include ./GNUSource.make

CXX := g++

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

Install_Target = install-clang
Build_Target = clang-only

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

# Remove the parts of the destroot we don't need
post-install:

OSV = $(DSTROOT)/usr/local/OpenSourceVersions
OSL = $(DSTROOT)/usr/local/OpenSourceLicenses

install-plist:
	$(MKDIR) $(OSV)
	$(INSTALL_FILE) $(SRCROOT)/$(Project).plist $(OSV)/$(Project).plist
	$(MKDIR) $(OSL)
	$(INSTALL_FILE) $(Sources)/LICENSE.TXT $(OSL)/$(Project)-llvm.txt
	$(INSTALL_FILE) $(Sources)/tools/clang/LICENSE.TXT $(OSL)/$(Project).txt

SVN_BASE := $(shell svn info | sed -n 's/^URL: //; s,/llvm-project/.*$$,/llvm-project,p')
SVN_CLANG := $(shell svn info | sed -n 's/^URL: //p')

update-clang:
	svn rm -m 'Update.' $(SVN_CLANG)/clang
	svn cp -m 'Update.' $(SVN_BASE)/llvm/trunk $(SVN_CLANG)/clang
	svn cp -m 'Update.' $(SVN_BASE)/cfe/trunk $(SVN_CLANG)/clang/tools/clang
