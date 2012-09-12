//===-- AMDILRegisterInfo.td ----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Declarations that describe the AMDIL register file.
//
//===----------------------------------------------------------------------===//

class AMDILRegWithSubReg<bits<16> num, string n, list<Register> subregs, list<SubRegIndex> subRegIdx>
: RegisterWithSubRegs<n, subregs> {
  field bits<16> Value;
  let Value = num;
  let Namespace = "AMDIL";
  let SubRegIndices = subRegIdx;
}
class AMDILReg<bits<16> num, string n> : Register<n> {
  field bits<16> Value;
  let Value = num;
  let Namespace = "AMDIL";
}

def sub_x_comp : SubRegIndex;
def sub_y_comp : SubRegIndex;
def sub_z_comp : SubRegIndex;
def sub_w_comp : SubRegIndex;
def sub_xy_comp : SubRegIndex;
def sub_zw_comp : SubRegIndex;

include "AMDILRegisterDefsScalar.td"
include "AMDILRegisterDefsV2.td"
include "AMDILRegisterDefsV4.td"

// All registers between 1000 and 1024 are reserved and cannot be used
// unless commented in this section
// r1021-r1039 are used to dynamically calculate the local/group/thread/region/region_local ID's
// r1020 is used to hold the frame index for local arrays
// r1019 is used to hold the dynamic stack allocation pointer
// r1018 is used as a temporary register for handwritten code
// r1017 is used as a temporary register for handwritten code
// r1016 is used as a temporary register for load/store code
// r1015 is used as a temporary register for data segment offset
// r1014 is used as a temporary register for store code
// r1013 is used as the section data pointer register
// r1012-r1010 and r1001-r1008 are used for temporary I/O registers
// r1028 is used as the frame pointer register
// r1027 is used as mem register
// r1026 is used as the return address register.
// r1030-r1039 are reserved for AMDILCFGStructurizer.
// r1040 is reserved for printf implementation.
//def R1025 : AMDILReg<1025, "r1025">, DwarfRegNum<[1025]>;
//def R1024 : AMDILReg<1024, "r1024">, DwarfRegNum<[1024]>;
//def R1023 : AMDILReg<1023, "r1023">, DwarfRegNum<[1023]>;
//def R1022 : AMDILReg<1022, "r1022">, DwarfRegNum<[1022]>;
//def R1021 : AMDILReg<1021, "r1021">, DwarfRegNum<[1021]>;
//def R1020 : AMDILReg<1020, "r1020">, DwarfRegNum<[1020]>;
def FP : AMDILReg<5012, "r1028">, DwarfRegNum<[1028]>;
def DFP : AMDILReg<5016, "r1029">, DwarfRegNum<[1029]>;
def SP : AMDILReg<4076, "r1019">, DwarfRegNum<[1019]>;
def T1 : AMDILReg<4072, "r1018">, DwarfRegNum<[1018]>;
def T2 : AMDILReg<4068, "r1017">, DwarfRegNum<[1017]>;
def T3 : AMDILReg<4064, "r1016">, DwarfRegNum<[1016]>;
def T4 : AMDILReg<4060, "r1015">, DwarfRegNum<[1015]>;
def T5 : AMDILReg<4056, "r1014">, DwarfRegNum<[1014]>;
def SDP : AMDILReg<4052, "r1013">, DwarfRegNum<[1013]>;
def MEMx : AMDILReg<5008,"mem0">, DwarfRegNum<[5008]>;
def MEMxy : AMDILReg<5008, "mem0">, DwarfRegAlias<MEMx>;
def MEMxyz : AMDILReg<5008, "mem0">, DwarfRegAlias<MEMx>;
def MEM   : AMDILReg<5008, "mem0">, DwarfRegAlias<MEMx>;
def RA : AMDILReg<5004, "r1026">, DwarfRegNum<[1026]>;
def CFG1 : AMDILReg<5020, "r1030">, DwarfRegNum<[1030]>;
def CFG2 : AMDILReg<5024, "r1031">, DwarfRegNum<[1031]>;
def CFG3 : AMDILReg<5028, "r1032">, DwarfRegNum<[1032]>;
def CFG4 : AMDILReg<5032, "r1033">, DwarfRegNum<[1033]>;
def CFG5 : AMDILReg<5036, "r1034">, DwarfRegNum<[1034]>;
def CFG6 : AMDILReg<5040, "r1035">, DwarfRegNum<[1035]>;
def CFG7 : AMDILReg<5044, "r1036">, DwarfRegNum<[1036]>;
def CFG8 : AMDILReg<5048, "r1037">, DwarfRegNum<[1037]>;
def CFG9 : AMDILReg<5052, "r1038">, DwarfRegNum<[1038]>;
def CFG10 : AMDILReg<5056, "r1039">, DwarfRegNum<[1039]>;
def PRINTF : AMDILReg<5060, "r1040">, DwarfRegNum<[1040]>;

def GPRI8 : RegisterClass<"AMDIL", [i8], 8,
    ( add include "AMDILRegisterUsesScalar.td"
    )>
{
        let AltOrders = [(add (sequence "Rx%u", 65, 191), (sequence "Ry%u", 65, 191), (sequence "Rz%u", 65, 191), (sequence "Rw%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRI16 : RegisterClass<"AMDIL", [i16], 16,
    ( add include "AMDILRegisterUsesScalar.td" )>
{
        let AltOrders = [(add (sequence "Rx%u", 65, 191), (sequence "Ry%u", 65, 191), (sequence "Rz%u", 65, 191), (sequence "Rw%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
            }
def GPRI32 : RegisterClass<"AMDIL", [i32], 32,
    ( add include "AMDILRegisterUsesScalar.td"
    , FP, RA, SDP, T5, MEMx
    )>
    {
        let AltOrders = [(add (sequence "Rx%u", 65, 191), (sequence "Ry%u", 65, 191), (sequence "Rz%u", 65, 191), (sequence "Rw%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRXI32 : RegisterClass<"AMDIL", [i32], 32,
    ( add include "AMDILRegisterUsesScalarX.td" )>
    {
        let AltOrders = [(add (sequence "Rx%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRYI32 : RegisterClass<"AMDIL", [i32], 32,
    ( add include "AMDILRegisterUsesScalarY.td" )>
    {
        let AltOrders = [(add (sequence "Ry%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRZI32 : RegisterClass<"AMDIL", [i32], 32,
    ( add include "AMDILRegisterUsesScalarZ.td" )>
    {
        let AltOrders = [(add (sequence "Rz%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRWI32 : RegisterClass<"AMDIL", [i32], 32,
    ( add include "AMDILRegisterUsesScalarW.td" )>
    {
        let AltOrders = [(add (sequence "Rw%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }

def GPRF32 : RegisterClass<"AMDIL", [f32], 32,
    ( add include "AMDILRegisterUsesScalar.td" )>
    {
        let AltOrders = [(add (sequence "Rx%u", 65, 191), (sequence "Ry%u", 65, 191), (sequence "Rz%u", 65, 191), (sequence "Rw%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRI64 : RegisterClass<"AMDIL", [i64], 64,
    ( add include "AMDILRegisterUsesV2.td" )>
    {
        let AltOrders = [(add (sequence "Rxy%u", 65, 191), (sequence "Rzw%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRXYI64 : RegisterClass<"AMDIL", [i64], 64,
    ( add include "AMDILRegisterUsesV2XY.td" )>
    {
        let AltOrders = [(add (sequence "Rxy%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRZWI64 : RegisterClass<"AMDIL", [i64], 64,
    ( add include "AMDILRegisterUsesV2ZW.td" )>
    {
        let AltOrders = [(add (sequence "Rzw%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRF64 : RegisterClass<"AMDIL", [f64], 64,
    ( add include "AMDILRegisterUsesV2.td" )>
    {
        let AltOrders = [(add (sequence "Rxy%u", 65, 191), (sequence "Rzw%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRV2I8 : RegisterClass<"AMDIL", [v2i8], 16,
    ( add include "AMDILRegisterUsesV2.td" )>
    {
        let AltOrders = [(add (sequence "Rxy%u", 65, 191), (sequence "Rzw%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRV2I16 : RegisterClass<"AMDIL", [v2i16], 32,
    ( add include "AMDILRegisterUsesV2.td" )>
    {
        let AltOrders = [(add (sequence "Rxy%u", 65, 191), (sequence "Rzw%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRV2I32 : RegisterClass<"AMDIL", [v2i32], 64,
    ( add include "AMDILRegisterUsesV2.td"
    , MEMxy
    )>
    {
        let AltOrders = [(add (sequence "Rxy%u", 65, 191), (sequence "Rzw%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRV2F32 : RegisterClass<"AMDIL", [v2f32], 64,
    ( add include "AMDILRegisterUsesV2.td" )>
    {
        let AltOrders = [(add (sequence "Rxy%u", 65, 191), (sequence "Rzw%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRV2I64 : RegisterClass<"AMDIL", [v2i64], 128,
    ( add include "AMDILRegisterUsesV4.td" )>
    {
        let AltOrders = [(add (sequence "R%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
def GPRV2F64 : RegisterClass<"AMDIL", [v2f64], 128,
    ( add include "AMDILRegisterUsesV4.td" )>
    {
        let AltOrders = [(add (sequence "R%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
  }];
}
def GPRV4I8 : RegisterClass<"AMDIL", [v4i8], 32,
    ( add include "AMDILRegisterUsesV4.td" )>
{
        let AltOrders = [(add (sequence "R%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
  }];
}
def GPRV4I16 : RegisterClass<"AMDIL", [v4i16], 64,
    ( add include "AMDILRegisterUsesV4.td" )>
{
        let AltOrders = [(add (sequence "R%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
  }];
}
def GPRV4F32 : RegisterClass<"AMDIL", [v4f32], 128,
    ( add include "AMDILRegisterUsesV4.td" )>
{
        let AltOrders = [(add (sequence "R%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
  }];
}
def GPRV4I32 : RegisterClass<"AMDIL", [v4i32], 128,
    ( add include "AMDILRegisterUsesV4.td"
    , SP, T1, T2, T3, T4, MEM
    )>
{
        let AltOrders = [(add (sequence "R%u", 65, 191))];
        let AltOrderSelect = [{
          return 1;
        }];
    }
