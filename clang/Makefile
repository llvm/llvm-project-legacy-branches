##
# Top-level Clang B&I Makefile
##

# LLVM/Clang default configuration.

# Select default assertion mode, valid values are '0' and '1'.
Clang_Use_Assertions   := 1
# Select clang driver mode, valid values are 'Production' and 'Development'.
Clang_Driver_Mode      := Production
# Select optimization flags to use.
Clang_Optimize_Option  := -O2
# Additional arbitrary compiler options.
Clang_Extra_Options    := -g -DDISABLE_SMART_POINTERS -DUDP_CODE_COMPLETION_LOGGER='\"17.224.14.219\"' -DUDP_CODE_COMPLETION_LOGGER_PORT=4080
# Enable use of clang++?
Clang_Enable_CXX       := 1
# Build all LLVM tools; not just clang?
Clang_Build_All        := 0

##
# Include build logic.
include ClangBNI.mk
