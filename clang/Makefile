##
# Clang Makefile
##

# Project info
Project                := objc_translate

##

CMAKE := $(CMAKE)
ifeq ($(strip $(CMAKE)),)
CMAKE := cmake
endif

VSVARS := $(VS100COMNTOOLS)\vsvars32.bat
MSBUILD := C:\WINDOWS\Microsoft.NET\Framework\v4.0.30319\msbuild.exe
JOBS := 1

ifeq ($(ENABLE_TESTS),1)
BUILD_TARGET := test
else
BUILD_TARGET := build
endif

##

SVN_BASE = $(shell svn info | sed -n "s/^URL: //; s,/llvm-project/.*$$,/llvm-project,p")
SVN_CLANG = $(shell svn info | sed -n "s/^URL: //p")
SVN_TAGS = $(SVN_BASE)/cfe/tags/Apple

# Instead of jiggling the project files to only install the bits we want, we do
# a real install into the OBJROOT, and then copy the exact bits we want to the
# DSTROOT.
#
# It would be nice to not build unnecessary stuff though, to save build time.
STAGEROOT := $(OBJROOT)\install

BINDEST := $(DSTROOT)\AppleInternal\bin

# Override default CMake flags to use static C run-time library.
MSVC_CRT := MT

# Override CMake default libraries list, which is much larger than we need.
CXXLIBS := kernel32.lib user32.lib advapi32.lib

TARGET := /p:Configuration=Release /p:Platform=Win32

all:: help

help:
	@echo "usage: make [target]"
	@echo ""
	@echo "Available Targets:"
	@echo "  clean    : clean the build (removes OBJROOT)"
	@echo "  configure: configure using CMake (inside OBJROOT)"
	@echo "  build    : build using vcbuild.exe (inside OBJROOT)"
	@echo "  test     : run the clang regression tests"
	@echo "  install  : configure, build, and install (set ENABLE_TESTS=1"
	@echo "             to also run tests)"
	@echo ""
	@echo "  update-sources: update LLVM and Clang sources"

clean:
	del /s/q "$(OBJROOT)"

configure:
	"$(CMAKE)" -E make_directory "$(OBJROOT)"
	cd "$(OBJROOT)" && \
	"$(CMAKE)" "-DCMAKE_INSTALL_PREFIX:=$(STAGEROOT)" \
	           "-DLLVM_TARGETS_TO_BUILD:=X86;ARM" \
	           "-DLLVM_USE_CRT_RELEASE:=$(MSVC_CRT)" \
	           "-DLLVM_USE_CRT_MINSIZEREL:=$(MSVC_CRT)" \
	           "-DLLVM_USE_CRT_RELWITHDEBINFO:=$(MSVC_CRT)" \
	           "-DCMAKE_CXX_STANDARD_LIBRARIES:STRING=$(CXXLIBS)" \
	           "-DLLVM_INCLUDE_EXAMPLES:=OFF" \
	           "-DLLVM_INCLUDE_TESTS:=OFF" \
	           "-DLLVM_BUILD_EXAMPLES:=OFF" \
	           "-DLLVM_BUILD_TESTS:=OFF" \
	           -G "Visual Studio 10" \
	           "$(SRCROOT)\src"

# FIXME: Only build clang.
build: configure
	"$(MSBUILD)" /maxcpucount:$(JOBS) "$(OBJROOT)"\LLVM.sln $(TARGET)
	"$(MSBUILD)" /maxcpucount:$(JOBS) "$(OBJROOT)"\INSTALL.vcxproj $(TARGET)

test: build
	"$(MSBUILD)" "$(OBJROOT)"\tools\clang\test\clang-test.vcxproj $(TARGET)

install: $(BUILD_TARGET)
	"$(CMAKE)" -E make_directory "$(BINDEST)"
	xcopy /i/y "$(STAGEROOT)\bin\clang.exe" "$(BINDEST)"

update-sources:
	svn rm -m 'Update.' $(SVN_CLANG)/src
	svn cp -m 'Update.' $(SVN_BASE)/llvm/trunk@$(REVISION) $(SVN_CLANG)/src
	svn cp -m 'Update.' $(SVN_BASE)/cfe/trunk@$(REVISION) $(SVN_CLANG)/src/tools/clang
	svn up

update-sources-from-tag:
	svn rm -m 'Update.' $(SVN_CLANG)/src
	svn cp -m 'Update.' $(SVN_BASE)/cfe/tags/Apple/clang-$(VERSION)/src $(SVN_CLANG)/src
	svn up

tag-objc_translate:
	svn cp -m 'Tag.' $(SVN_CLANG) $(SVN_TAGS)/objc_translate-$(VERSION)

retag-objc_translate:
	svn rm -m 'Retag.' $(SVN_TAGS)/objc_translate-$(VERSION) && \
	svn cp -m 'Retag.' $(SVN_CLANG) $(SVN_TAGS)/objc_translate-$(VERSION); \

###
# Debugging

make-print-%:
	$(error PRINT: $(value $*) = "$($*)" (from $(origin $*)))
