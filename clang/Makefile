##
# Top-level Clang B&I Makefile
##

# LLVM/Clang default configuration.

# Select default assertion mode, valid values are '0' and '1'.
Clang_Use_Assertions   := 0
# Select clang driver mode, valid values are 'Production' and 'Development'.
Clang_Driver_Mode      := Production
# Select optimization flags to use.
Clang_Optimize_Option  := -O2
# Additional arbitrary compiler options.
Clang_Extra_Options    := -DDISABLE_SMART_POINTERS -DDISABLE_DEFAULT_INTEGRATED_ASSEMBLER
# Additional arbitrary compiler options, only passed to final configure stage.
Clang_Final_Extra_Options := -g
# Enable use of clang++?
Clang_Enable_CXX       := 0
# Build all LLVM tools; not just clang?
Clang_Build_All        := 0
# Set the Clang version.
Clang_Version          := 1.6
# Never bootstrap.
Clang_Enable_Bootstrap := 0

##
# Source Info

# The name used to identify this "train".
Train_Name := williamson
# Can be one of "trunk", "branch", or "branch-llvm-only".
Source_To_Draw_From := branch

##
# Include build logic.
include ClangBNI.mk
