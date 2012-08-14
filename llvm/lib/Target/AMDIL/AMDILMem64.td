//===-- AMDILMem64.td -----------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//

let Predicates = [Has64BitPtr] in {
  let isCodeGenOnly=1 in {
    //===----------------------------------------------------------------------===//
    // Store Memory Operations
    //===----------------------------------------------------------------------===//
    defm GLOBALTRUNCSTORE64 : GTRUNCSTORE64<"!global trunc store">;
    defm GLOBAL              : STORE64<"!global store"         , global_store>;
    defm LOCALTRUNCSTORE64  : LTRUNCSTORE64<"!local trunc store">;
    defm LOCAL               : STORE64<"!local store"          , local_store>;
    defm PRIVATETRUNCSTORE64 : PTRUNCSTORE64<"!private trunc store">;
    defm PRIVATE             : STORE64<"!private store"        , private_store>;
    defm REGIONTRUNCSTORE64 : RTRUNCSTORE64<"!region trunc store">;
    defm REGION              : STORE64<"!region hw store"      , region_store>;


    //===---------------------------------------------------------------------===//
    // Load Memory Operations
    //===---------------------------------------------------------------------===//
    defm GLOBAL       : LOAD64<"!global load"            , global_load>;
    defm GLOBALZEXT   : LOAD64<"!global zext load"       , global_zext_load>;
    defm GLOBALSEXT   : LOAD64<"!global sext load"       , global_sext_load>;
    defm GLOBALAEXT   : LOAD64<"!global aext load"       , global_aext_load>;
    defm PRIVATE      : LOAD64<"!private load"           , private_load>;
    defm PRIVATEZEXT  : LOAD64<"!private zext load"      , private_zext_load>;
    defm PRIVATESEXT  : LOAD64<"!private sext load"      , private_sext_load>;
    defm PRIVATEAEXT  : LOAD64<"!private aext load"      , private_aext_load>;
    defm CPOOL        : LOAD64<"!constant pool load"     , cp_load>;
    defm CPOOLZEXT    : LOAD64<"!constant pool zext load", cp_zext_load>;
    defm CPOOLSEXT    : LOAD64<"!constant pool sext load", cp_sext_load>;
    defm CPOOLAEXT    : LOAD64<"!constant aext pool load", cp_aext_load>;
    defm CONSTANT     : LOAD64<"!constant load"          , constant_load>;
    defm CONSTANTZEXT : LOAD64<"!constant zext load"     , constant_zext_load>;
    defm CONSTANTSEXT : LOAD64<"!constant sext load"     , constant_sext_load>;
    defm CONSTANTAEXT : LOAD64<"!constant aext load"     , constant_aext_load>;
    defm LOCAL        : LOAD64<"!local load"             , local_load>;
    defm LOCALZEXT    : LOAD64<"!local zext load"        , local_zext_load>;
    defm LOCALSEXT    : LOAD64<"!local sext load"        , local_sext_load>;
    defm LOCALAEXT    : LOAD64<"!local aext load"        , local_aext_load>;
    defm REGION       : LOAD64<"!region load"            , region_load>;
    defm REGIONZEXT   : LOAD64<"!region zext load"       , region_zext_load>;
    defm REGIONSEXT   : LOAD64<"!region sext load"       , region_sext_load>;
    defm REGIONAEXT   : LOAD64<"!region aext load"       , region_aext_load>;
  }
  //===---------------------------------------------------------------------===//
  // IO Expansion Load/Store Instructions
  //===---------------------------------------------------------------------===//
  // These opcode has custom swizzle patterns for some of the arguments.
  let mayLoad = 1 in {
    def UAVRAWLOAD64_i32 : TwoInOneOut<IL_OP_RAW_UAV_LOAD, (outs GPRI32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_RAW_UAV_LOAD.Text, "_addr(64)_id($id) $dst, $addy"), []>;
    def UAVRAWLOAD64_v2i32 : TwoInOneOut<IL_OP_RAW_UAV_LOAD, (outs GPRV2I32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_RAW_UAV_LOAD.Text, "_addr(64)_id($id) $dst, $addy"), []>;
    def UAVRAWLOAD64_v4i32 : TwoInOneOut<IL_OP_RAW_UAV_LOAD, (outs GPRV4I32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_RAW_UAV_LOAD.Text, "_addr(64)_id($id) $dst, $addy"), []>;
    def UAVRAWLOADCACHED64_i32 : TwoInOneOut<IL_OP_RAW_UAV_LOAD, (outs GPRI32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_RAW_UAV_LOAD.Text, "_addr(64)_id($id)_cached $dst, $addy"), []>;
    def UAVRAWLOADCACHED64_v2i32 : TwoInOneOut<IL_OP_RAW_UAV_LOAD, (outs GPRV2I32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_RAW_UAV_LOAD.Text, "_addr(64)_id($id)_cached $dst, $addy"), []>;
    def UAVRAWLOADCACHED64_v4i32 : TwoInOneOut<IL_OP_RAW_UAV_LOAD, (outs GPRV4I32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_RAW_UAV_LOAD.Text, "_addr(64)_id($id)_cached $dst, $addy"), []>;
    def UAVRAWLOADCACHEDALIGNED64_v2i32 : TwoInOneOut<IL_OP_RAW_UAV_LOAD, (outs GPRV2I32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_RAW_UAV_LOAD.Text, "_addr(64)_id($id)_cached_aligned $dst, $addy"), []>;
    def UAVRAWLOADCACHEDALIGNED64_v4i32 : TwoInOneOut<IL_OP_RAW_UAV_LOAD, (outs GPRV4I32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_RAW_UAV_LOAD.Text, "_addr(64)_id($id)_cached_aligned $dst, $addy"), []>;
    let Predicates = [HasByteShortUAV] in {
      def UAVRAWLOAD64_i8 : TwoInOneOut<IL_OP_UAV_BYTE_LOAD, (outs GPRI32:$dst),
          (ins GPRXI32:$addy, i64imm:$id),
          !strconcat(IL_OP_UAV_BYTE_LOAD.Text, "_addr(64)_id($id) $dst, $addy"), []>;
      def UAVRAWLOAD64_u8 : TwoInOneOut<IL_OP_UAV_UBYTE_LOAD, (outs GPRI32:$dst),
          (ins GPRXI32:$addy, i64imm:$id),
          !strconcat(IL_OP_UAV_UBYTE_LOAD.Text, "_addr(64)_id($id) $dst, $addy"), []>;
      def UAVRAWLOAD64_i16 : TwoInOneOut<IL_OP_UAV_SHORT_LOAD, (outs GPRI32:$dst),
          (ins GPRXI32:$addy, i64imm:$id),
          !strconcat(IL_OP_UAV_SHORT_LOAD.Text, "_addr(64)_id($id) $dst, $addy"), []>;
      def UAVRAWLOAD64_u16 : TwoInOneOut<IL_OP_UAV_USHORT_LOAD, (outs GPRI32:$dst),
          (ins GPRXI32:$addy, i64imm:$id),
          !strconcat(IL_OP_UAV_USHORT_LOAD.Text, "_addr(64)_id($id) $dst, $addy"), []>;
      def UAVRAWSTORE64_i8 : TwoInOneOut<IL_OP_UAV_BYTE_STORE, (outs GPRI32:$mem),
          (ins GPRXI32:$addy, GPRI32:$src, i64imm:$id),
          !strconcat(IL_OP_UAV_BYTE_STORE.Text, "_addr(64)_id($id) $mem, $addy, $src"), []>;
      def UAVRAWSTORE64_i16 : TwoInOneOut<IL_OP_UAV_SHORT_STORE, (outs GPRI32:$mem),
          (ins GPRXI32:$addy, GPRI32:$src, i64imm:$id),
          !strconcat(IL_OP_UAV_SHORT_STORE.Text, "_addr(64)_id($id) $mem, $addy, $src"), []>;
      def UAVRAWLOADCACHED64_i8 : TwoInOneOut<IL_OP_UAV_BYTE_LOAD, (outs GPRI32:$dst),
          (ins GPRXI32:$addy, i64imm:$id),
          !strconcat(IL_OP_UAV_BYTE_LOAD.Text, "_addr(64)_id($id)_cached $dst, $addy"), []>;
      def UAVRAWLOADCACHED64_u8 : TwoInOneOut<IL_OP_UAV_UBYTE_LOAD, (outs GPRI32:$dst),
          (ins GPRXI32:$addy, i64imm:$id),
          !strconcat(IL_OP_UAV_UBYTE_LOAD.Text, "_addr(64)_id($id)_cached $dst, $addy"), []>;
      def UAVRAWLOADCACHED64_i16 : TwoInOneOut<IL_OP_UAV_SHORT_LOAD, (outs GPRI32:$dst),
          (ins GPRXI32:$addy, i64imm:$id),
          !strconcat(IL_OP_UAV_SHORT_LOAD.Text, "_addr(64)_id($id)_cached $dst, $addy"), []>;
      def UAVRAWLOADCACHED64_u16 : TwoInOneOut<IL_OP_UAV_USHORT_LOAD, (outs GPRI32:$dst),
          (ins GPRXI32:$addy, i64imm:$id),
          !strconcat(IL_OP_UAV_USHORT_LOAD.Text, "_addr(64)_id($id)_cached $dst, $addy"), []>;
    }
    def SCRATCHLOAD64 : TwoInOneOut<IL_OP_MOV, (outs GPRV4I32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_MOV.Text, " $dst, x$id[$addy]"), []>;
    def CBLOAD64 : TwoInOneOut<IL_OP_MOV, (outs GPRV4I32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_MOV.Text, " $dst, cb$id[$addy]"), []>;
    def GDSLOAD64 : TwoInOneOut<IL_OP_GDS_LOAD, (outs GPRI32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_GDS_LOAD.Text, "_id($id) $dst, $addy"), []>;
    def GDSLOAD64_i8 : TwoInOneOut<IL_OP_GDS_LOAD_BYTE, (outs GPRI32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_GDS_LOAD_BYTE.Text, "_id($id) $dst, $addy"), []>;
    def GDSLOAD64_u8 : TwoInOneOut<IL_OP_GDS_LOAD_UBYTE, (outs GPRI32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_GDS_LOAD_UBYTE.Text, "_id($id) $dst, $addy"), []>;
    def GDSLOAD64_i16 : TwoInOneOut<IL_OP_GDS_LOAD_SHORT, (outs GPRI32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_GDS_LOAD_SHORT.Text, "_id($id) $dst, $addy"), []>;
    def GDSLOAD64_u16 : TwoInOneOut<IL_OP_GDS_LOAD_USHORT, (outs GPRI32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_GDS_LOAD_USHORT.Text, "_id($id) $dst, $addy"), []>;

    def LDSLOADVEC64 : TwoInOneOut<IL_OP_LDS_LOAD_VEC, (outs GPRI32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_LDS_LOAD_VEC.Text, "_id($id) $dst, $addy, $addy"), []>;
    def LDSLOADVEC64_v2i32 : TwoInOneOut<IL_OP_LDS_LOAD_VEC, (outs GPRV2I32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_LDS_LOAD_VEC.Text, "_id($id) $dst, $addy, $addy"), []>;
    def LDSLOADVEC64_v4i32 : TwoInOneOut<IL_OP_LDS_LOAD_VEC, (outs GPRV4I32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_LDS_LOAD_VEC.Text, "_id($id) $dst, $addy, $addy"), []>;
    def LDSLOAD64 : TwoInOneOut<IL_OP_LDS_LOAD, (outs GPRI32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_LDS_LOAD.Text, "_id($id) $dst, $addy"), []>;
    def LDSLOAD64_i8 : TwoInOneOut<IL_OP_LDS_LOAD_BYTE, (outs GPRI32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_LDS_LOAD_BYTE.Text, "_id($id) $dst, $addy"), []>;
    def LDSLOAD64_u8 : TwoInOneOut<IL_OP_LDS_LOAD_UBYTE, (outs GPRI32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_LDS_LOAD_UBYTE.Text, "_id($id) $dst, $addy"), []>;
    def LDSLOAD64_i16 : TwoInOneOut<IL_OP_LDS_LOAD_SHORT, (outs GPRI32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_LDS_LOAD_SHORT.Text, "_id($id) $dst, $addy"), []>;
    def LDSLOAD64_u16 : TwoInOneOut<IL_OP_LDS_LOAD_USHORT, (outs GPRI32:$dst),
        (ins GPRXI32:$addy, i64imm:$id),
        !strconcat(IL_OP_LDS_LOAD_USHORT.Text, "_id($id) $dst, $addy"), []>;

  }
  let mayStore = 1 in {
    def UAVRAWSTORE64_i32 : TwoInOneOut<IL_OP_RAW_UAV_STORE, (outs GPRI32:$mem),
        (ins GPRXI32:$addy, GPRI32:$src, i64imm:$id),
        !strconcat(IL_OP_RAW_UAV_STORE.Text, "_addr(64)_id($id) $mem, $addy, $src"), []>;
    def UAVRAWSTORE64_v2i32 : TwoInOneOut<IL_OP_RAW_UAV_STORE, (outs GPRV2I32:$mem),
        (ins GPRXI32:$addy, GPRV2I32:$src, i64imm:$id),
        !strconcat(IL_OP_RAW_UAV_STORE.Text, "_addr(64)_id($id) $mem, $addy, $src"), []>;
    def UAVRAWSTORE64_v4i32 : TwoInOneOut<IL_OP_RAW_UAV_STORE, (outs GPRV4I32:$mem),
        (ins GPRXI32:$addy, GPRV4I32:$src, i64imm:$id),
        !strconcat(IL_OP_RAW_UAV_STORE.Text, "_addr(64)_id($id) $mem, $addy, $src"), []>;
    def SCRATCHSTORE64 : TwoInOneOut<IL_OP_MOV, (outs GPRXI32:$addy),
        (ins GPRV4I32:$data, i64imm:$id),
        !strconcat(IL_OP_MOV.Text, " x$id[$addy, $data"), []>;
    def GDSSTORE64 : TwoInOneOut<IL_OP_GDS_STORE, (outs GPRXI32:$addy),
        (ins GPRI32:$src, i64imm:$id),
        !strconcat(IL_OP_GDS_STORE.Text, "_id($id) $addy, $src"), []>;
    def LDSSTOREVEC64 : ThreeInOneOut<IL_OP_LDS_STORE_VEC, (outs GPRI32:$mem),
        (ins GPRXI32:$addy, GPRI32:$src, i64imm:$id),
        !strconcat(IL_OP_LDS_STORE_VEC.Text, "_id($id) $mem, $addy, $src, $src"), []>;
    def LDSSTOREVEC64_v2i32 : ThreeInOneOut<IL_OP_LDS_STORE_VEC, (outs GPRV2I32:$mem),
        (ins GPRXI32:$addy, GPRV2I32:$src, i64imm:$id),
        !strconcat(IL_OP_LDS_STORE_VEC.Text, "_id($id) $mem, $addy, $src, $src"), []>;
    def LDSSTOREVEC64_v4i32 : ThreeInOneOut<IL_OP_LDS_STORE_VEC, (outs GPRV4I32:$mem),
        (ins GPRXI32:$addy, GPRV4I32:$src, i64imm:$id),
        !strconcat(IL_OP_LDS_STORE_VEC.Text, "_id($id) $mem, $addy, $src, $src"), []>;
    def LDSSTORE64 : TwoInOneOut<IL_OP_LDS_STORE, (outs GPRXI32:$addy),
        (ins GPRI32:$src, i64imm:$id),
        !strconcat(IL_OP_LDS_STORE.Text, "_id($id) $addy, $src"), []>;
    def LDSSTORE64_i8 : TwoInOneOut<IL_OP_LDS_STORE_BYTE, (outs GPRXI32:$addy),
        (ins GPRI32:$src, i64imm:$id),
        !strconcat(IL_OP_LDS_STORE_BYTE.Text, "_id($id) $addy, $src"), []>;
    def LDSSTORE64_i16 : TwoInOneOut<IL_OP_LDS_STORE_SHORT, (outs GPRXI32:$addy),
        (ins GPRI32:$src, i64imm:$id),
        !strconcat(IL_OP_LDS_STORE_SHORT.Text, "_id($id) $addy, $src"), []>;
  }
  // Image related operations.
  def IMAGE1D64_READ : ILFormat<IL_OP_SAMPLE, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr, GPRI32:$sampler, GPRV4F32:$addy),
      !strconcat(IL_OP_SAMPLE.Text,
          "_id($ptr)_sampler($sampler)_coordtype(normalized) $dst, $addy"),
      [(set GPRV4I32:$dst,
          (int_AMDIL_image1d_read_norm ADDR64:$ptr, GPRI32:$sampler, GPRV4F32:$addy))]>;
  def IMAGE1DA64_READ : ILFormat<IL_OP_SAMPLE, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr, GPRI32:$sampler, GPRV4F32:$addy),
      !strconcat(IL_OP_SAMPLE.Text,
          "_id($ptr)_sampler($sampler)_coordtype(normalized) $dst, $addy"),
      [(set GPRV4I32:$dst,
          (int_AMDIL_image1d_array_read_norm ADDR64:$ptr, GPRI32:$sampler, GPRV4F32:$addy))]>;
  def IMAGE1DB64_TXLD : ILFormat<IL_OP_LOAD, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr, GPRI32:$addy),
      !strconcat(IL_OP_LOAD.Text, 
          "_id($ptr) $dst, $addy"),
      [(set GPRV4I32:$dst,
          (int_AMDIL_image1d_buffer_load ADDR64:$ptr, GPRI32:$addy))]>;
  def IMAGE1DB64_READ : ILFormat<IL_OP_SAMPLE, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr, GPRI32:$sampler, GPRV4F32:$addy),
      !strconcat(IL_OP_SAMPLE.Text, 
          "_id($ptr)_sampler($sampler)_coordtype(normalized) $dst, $addy"),
      [(set GPRV4I32:$dst,
          (int_AMDIL_image1d_buffer_read_norm ADDR64:$ptr, GPRI32:$sampler, GPRV4F32:$addy))]>;
  def IMAGE2D64_READ : ILFormat<IL_OP_SAMPLE, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr, GPRI32:$sampler, GPRV4F32:$addy),
      !strconcat(IL_OP_SAMPLE.Text,
          "_id($ptr)_sampler($sampler)_coordtype(normalized) $dst, $addy"),
      [(set GPRV4I32:$dst,
          (int_AMDIL_image2d_read_norm ADDR64:$ptr, GPRI32:$sampler, GPRV4F32:$addy))]>;
  def IMAGE2DA64_READ : ILFormat<IL_OP_SAMPLE, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr, GPRI32:$sampler, GPRV4F32:$addy),
      !strconcat(IL_OP_SAMPLE.Text,
          "_id($ptr)_sampler($sampler)_coordtype(normalized) $dst, $addy"),
      [(set GPRV4I32:$dst,
          (int_AMDIL_image2d_array_read_norm ADDR64:$ptr, GPRI32:$sampler, GPRV4F32:$addy))]>;
  def IMAGE3D64_READ : ILFormat<IL_OP_SAMPLE, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr, GPRI32:$sampler, GPRV4F32:$addy),
      !strconcat(IL_OP_SAMPLE.Text,
          "_id($ptr)_sampler($sampler)_coordtype(normalized) $dst, $addy"),
      [(set GPRV4I32:$dst,
          (int_AMDIL_image3d_read_norm ADDR64:$ptr, GPRI32:$sampler, GPRV4F32:$addy))]>;
  def IMAGE1D64_READ_UNNORM : ILFormat<IL_OP_SAMPLE, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr, GPRI32:$sampler, GPRV4F32:$addy),
      !strconcat(IL_OP_SAMPLE.Text,
          "_id($ptr)_sampler($sampler)_coordtype(unnormalized) $dst, $addy"),
      [(set GPRV4I32:$dst,
          (int_AMDIL_image1d_read_unnorm ADDR64:$ptr, GPRI32:$sampler, GPRV4F32:$addy))]>;
  def IMAGE1DA64_READ_UNNORM : ILFormat<IL_OP_SAMPLE, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr, GPRI32:$sampler, GPRV4F32:$addy),
      !strconcat(IL_OP_SAMPLE.Text,
          "_id($ptr)_sampler($sampler)_coordtype(unnormalized) $dst, $addy"),
      [(set GPRV4I32:$dst,
          (int_AMDIL_image1d_array_read_unnorm ADDR64:$ptr, GPRI32:$sampler, GPRV4F32:$addy))]>;
  def IMAGE1DB64_READ_UNNORM : ILFormat<IL_OP_SAMPLE, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr, GPRI32:$sampler, GPRV4F32:$addy),
      !strconcat(IL_OP_SAMPLE.Text, 
          "_id($ptr)_sampler($sampler)_coordtype(unnormalized) $dst, $addy"),
      [(set GPRV4I32:$dst,
          (int_AMDIL_image1d_buffer_read_unnorm ADDR64:$ptr, GPRI32:$sampler, GPRV4F32:$addy))]>;
  def IMAGE2D64_READ_UNNORM : ILFormat<IL_OP_SAMPLE, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr, GPRI32:$sampler, GPRV4F32:$addy),
      !strconcat(IL_OP_SAMPLE.Text,
          "_id($ptr)_sampler($sampler)_coordtype(unnormalized) $dst, $addy"),
      [(set GPRV4I32:$dst,
          (int_AMDIL_image2d_read_unnorm ADDR64:$ptr, GPRI32:$sampler, GPRV4F32:$addy))]>;
  def IMAGE2DA64_READ_UNNORM : ILFormat<IL_OP_SAMPLE, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr, GPRI32:$sampler, GPRV4F32:$addy),
      !strconcat(IL_OP_SAMPLE.Text,
          "_id($ptr)_sampler($sampler)_coordtype(unnormalized) $dst, $addy"),
      [(set GPRV4I32:$dst,
          (int_AMDIL_image2d_array_read_unnorm ADDR64:$ptr, GPRI32:$sampler, GPRV4F32:$addy))]>;
  def IMAGE3D64_READ_UNNORM : ILFormat<IL_OP_SAMPLE, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr, GPRI32:$sampler, GPRV4F32:$addy),
      !strconcat(IL_OP_SAMPLE.Text,
          "_id($ptr)_sampler($sampler)_coordtype(unnormalized) $dst, $addy"),
      [(set GPRV4I32:$dst,
          (int_AMDIL_image3d_read_unnorm ADDR64:$ptr, GPRI32:$sampler, GPRV4F32:$addy))]>;
  def IMAGE1D64_INFO0 : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr),
      !strconcat(IL_OP_MOV.Text, " $dst, $ptr"),
      [(set GPRV4I32:$dst, (int_AMDIL_image1d_info0 ADDR64:$ptr))]>;
  def IMAGE1D64_INFO1 : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr),
      !strconcat(IL_OP_MOV.Text, " $dst, $ptr"),
      [(set GPRV4I32:$dst, (int_AMDIL_image1d_info1 ADDR64:$ptr))]>;
  def IMAGE1DA64_INFO0 : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr),
      !strconcat(IL_OP_MOV.Text, " $dst, $ptr"),
      [(set GPRV4I32:$dst, (int_AMDIL_image1d_array_info0 ADDR64:$ptr))]>;
  def IMAGE1DA64_INFO1 : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr),
      !strconcat(IL_OP_MOV.Text, " $dst, $ptr"),
      [(set GPRV4I32:$dst, (int_AMDIL_image1d_array_info1 ADDR64:$ptr))]>;
  def IMAGE1DB64_INFO0 : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr),
      !strconcat(IL_OP_MOV.Text, " $dst, $ptr"),
      [(set GPRV4I32:$dst, (int_AMDIL_image1d_buffer_info0 ADDR64:$ptr))]>;
  def IMAGE1DB64_INFO1 : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr),
      !strconcat(IL_OP_MOV.Text, " $dst, $ptr"),
      [(set GPRV4I32:$dst, (int_AMDIL_image1d_buffer_info1 ADDR64:$ptr))]>;
  def IMAGE2DA64_INFO0 : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr),
      !strconcat(IL_OP_MOV.Text, " $dst, $ptr"),
      [(set GPRV4I32:$dst, (int_AMDIL_image2d_array_info0 ADDR64:$ptr))]>;
  def IMAGE2DA64_INFO1 : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr),
      !strconcat(IL_OP_MOV.Text, " $dst, $ptr"),
      [(set GPRV4I32:$dst, (int_AMDIL_image2d_array_info1 ADDR64:$ptr))]>;
  def IMAGE2D64_INFO0 : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr),
      !strconcat(IL_OP_MOV.Text, " $dst, $ptr"),
      [(set GPRV4I32:$dst, (int_AMDIL_image2d_info0 ADDR64:$ptr))]>;
  def IMAGE2D64_INFO1 : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr),
      !strconcat(IL_OP_MOV.Text, " $dst, $ptr"),
      [(set GPRV4I32:$dst, (int_AMDIL_image2d_info1 ADDR64:$ptr))]>;
  def IMAGE3D64_INFO0 : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr),
      !strconcat(IL_OP_MOV.Text, " $dst, $ptr"),
      [(set GPRV4I32:$dst, (int_AMDIL_image3d_info0 ADDR64:$ptr))]>;
  def IMAGE3D64_INFO1 : ILFormat<IL_OP_MOV, (outs GPRV4I32:$dst),
      (ins MEM6464:$ptr),
      !strconcat(IL_OP_MOV.Text, " $dst, $ptr"),
      [(set GPRV4I32:$dst, (int_AMDIL_image3d_info1 ADDR64:$ptr))]>;
  def IMAGE1D64_WRITE : ILFormat<IL_OP_UAV_STORE, (outs),
      (ins MEM6464:$ptr, GPRV2I32:$addy, GPRV4I32:$data),
      !strconcat(IL_OP_UAV_STORE.Text,
          "_id($ptr) $addy, $data"),
      [(int_AMDIL_image1d_write ADDR64:$ptr, GPRV2I32:$addy, GPRV4I32:$data)]>;
  def IMAGE1DA64_WRITE : ILFormat<IL_OP_UAV_STORE, (outs),
      (ins MEM6464:$ptr, GPRV2I32:$addy, GPRV4I32:$data),
      !strconcat(IL_OP_UAV_STORE.Text,
          "_id($ptr) $addy, $data"),
      [(int_AMDIL_image1d_array_write ADDR64:$ptr, GPRV2I32:$addy, GPRV4I32:$data)]>;
  def IMAGE1DB64_WRITE : ILFormat<IL_OP_UAV_STORE, (outs),
      (ins MEM6464:$ptr, GPRV2I32:$addy, GPRV4I32:$data),
      !strconcat(IL_OP_UAV_STORE.Text,
          "_id($ptr) $addy, $data"),
      [(int_AMDIL_image1d_buffer_write ADDR64:$ptr, GPRV2I32:$addy, GPRV4I32:$data)]>;
  def IMAGE2D64_WRITE : ILFormat<IL_OP_UAV_STORE, (outs),
      (ins MEM6464:$ptr, GPRV2I32:$addy, GPRV4I32:$data),
      !strconcat(IL_OP_UAV_STORE.Text,
          "_id($ptr) $addy, $data"),
      [(int_AMDIL_image2d_write ADDR64:$ptr, GPRV2I32:$addy, GPRV4I32:$data)]>;
  def IMAGE2DA64_WRITE : ILFormat<IL_OP_UAV_STORE, (outs),
      (ins MEM6464:$ptr, GPRV4I32:$addy, GPRV4I32:$data),
      !strconcat(IL_OP_UAV_STORE.Text,
          "_id($ptr) $addy, $data"),
      [(int_AMDIL_image2d_array_write ADDR64:$ptr, GPRV4I32:$addy, GPRV4I32:$data)]>;
  def IMAGE3D64_WRITE : ILFormat<IL_OP_UAV_STORE, (outs),
      (ins MEM6464:$ptr, GPRV4I32:$addy, GPRV4I32:$data),
      !strconcat(IL_OP_UAV_STORE.Text,
          "_id($ptr) $addy, $data"),
      [(int_AMDIL_image3d_write ADDR64:$ptr, GPRV4I32:$addy, GPRV4I32:$data)]>;


  // Printf related operations.
  let isCall=1, isAsCheapAsAMove = 1 in {
    def GET_PRINTF_OFFSET_i64: ILFormat<IL_OP_MOV, (outs GPRXYI64:$dst),
        (ins), !strconcat(IL_OP_MOV.Text, " $dst, cb0[8].zw"),
        [(set GPRXYI64:$dst, (int_AMDIL_get_printf_offset))]>;
    def GET_PRINTF_SIZE_i64 : ILFormat<IL_OP_MOV, (outs GPRXYI64:$dst),
        (ins), !strconcat(IL_OP_MOV.Text, " $dst, cb0[9].x0"),
        [(set GPRXYI64:$dst, (int_AMDIL_get_printf_size))]>;
  }
    // Atomic operations
  def ATOM64_G_ADD_NORET     : BinAtomNoRet64<IL_OP_UAV_ADD,
      "_addr(64)_id($id)", atom_g_add_noret>;
  def ATOM64_G_AND_NORET     : BinAtomNoRet64<IL_OP_UAV_AND,
      "_addr(64)_id($id)", atom_g_and_noret>;
  def ATOM64_G_MAX_NORET     : BinAtomNoRet64<IL_OP_UAV_MAX,
      "_addr(64)_id($id)", atom_g_max_noret>;
  def ATOM64_G_MIN_NORET     : BinAtomNoRet64<IL_OP_UAV_MIN,
      "_addr(64)_id($id)", atom_g_min_noret>;
  def ATOM64_G_UMAX_NORET    : BinAtomNoRet64<IL_OP_UAV_UMAX,
      "_addr(64)_id($id)", atom_g_umax_noret>;
  def ATOM64_G_UMIN_NORET    : BinAtomNoRet64<IL_OP_UAV_UMIN,
      "_addr(64)_id($id)", atom_g_umin_noret>;
  def ATOM64_G_OR_NORET      : BinAtomNoRet64<IL_OP_UAV_OR,
      "_addr(64)_id($id)", atom_g_or_noret>;
  def ATOM64_G_RSUB_NORET    : BinAtomNoRet64<IL_OP_UAV_RSUB,
      "_addr(64)_id($id)", atom_g_rsub_noret>;
  def ATOM64_G_SUB_NORET     : BinAtomNoRet64<IL_OP_UAV_SUB,
      "_addr(64)_id($id)", atom_g_sub_noret>;
  def ATOM64_G_XOR_NORET     : BinAtomNoRet64<IL_OP_UAV_XOR,
      "_addr(64)_id($id)", atom_g_xor_noret>;
  def ATOM64_G_INC_NORET     : BinAtomNoRet64<IL_OP_UAV_INC,
      "_addr(64)_id($id)", atom_g_inc_noret>;
  def ATOM64_G_DEC_NORET     : BinAtomNoRet64<IL_OP_UAV_DEC,
      "_addr(64)_id($id)", atom_g_dec_noret>;
  def ATOM64_G_CMPXCHG_NORET    : CmpXChgNoRet64<IL_OP_UAV_CMP,
      "_addr(64)_id($id)", atom_g_cmpxchg_noret>;
  def ATOM64_L_ADD_NORET     : BinAtomNoRet64<IL_OP_LDS_ADD,
      "_id($id)", atom_l_add_noret>;
  def ATOM64_L_AND_NORET     : BinAtomNoRet64<IL_OP_LDS_AND,
      "_id($id)", atom_l_and_noret>;
  def ATOM64_L_MAX_NORET     : BinAtomNoRet64<IL_OP_LDS_MAX,
      "_id($id)", atom_l_max_noret>;
  def ATOM64_L_MIN_NORET     : BinAtomNoRet64<IL_OP_LDS_MIN,
      "_id($id)", atom_l_min_noret>;
  def ATOM64_L_UMAX_NORET    : BinAtomNoRet64<IL_OP_LDS_UMAX,
      "_id($id)", atom_l_umax_noret>;
  def ATOM64_L_UMIN_NORET    : BinAtomNoRet64<IL_OP_LDS_UMIN,
      "_id($id)", atom_l_umin_noret>;
  def ATOM64_L_MSKOR_NORET   : TriAtomNoRet64<IL_OP_LDS_MSKOR,
      "_id($id)", atom_l_mskor_noret>;
  def ATOM64_L_OR_NORET      : BinAtomNoRet64<IL_OP_LDS_OR,
      "_id($id)", atom_l_or_noret>;
  def ATOM64_L_RSUB_NORET    : BinAtomNoRet64<IL_OP_LDS_RSUB,
      "_id($id)", atom_l_rsub_noret>;
  def ATOM64_L_SUB_NORET     : BinAtomNoRet64<IL_OP_LDS_SUB,
      "_id($id)", atom_l_sub_noret>;
  def ATOM64_L_XOR_NORET     : BinAtomNoRet64<IL_OP_LDS_XOR,
      "_id($id)", atom_l_xor_noret>;
  def ATOM64_L_INC_NORET     : BinAtomNoRet64<IL_OP_LDS_INC,
      "_id($id)", atom_l_inc_noret>;
  def ATOM64_L_DEC_NORET     : BinAtomNoRet64<IL_OP_LDS_DEC,
      "_id($id)", atom_l_dec_noret>;
  def ATOM64_L_CMPXCHG_NORET    : TriAtomNoRet64<IL_OP_LDS_CMP,
      "_id($id)", atom_l_cmpxchg_noret>;
  def ATOM64_R_ADD_NORET     : BinAtomNoRet64<IL_OP_GDS_ADD,
      "_id($id)", atom_r_add_noret>;
  def ATOM64_R_AND_NORET     : BinAtomNoRet64<IL_OP_GDS_AND,
      "_id($id)", atom_r_and_noret>;
  def ATOM64_R_MAX_NORET     : BinAtomNoRet64<IL_OP_GDS_MAX,
      "_id($id)", atom_r_max_noret>;
  def ATOM64_R_MIN_NORET     : BinAtomNoRet64<IL_OP_GDS_MIN,
      "_id($id)", atom_r_min_noret>;
  def ATOM64_R_UMAX_NORET    : BinAtomNoRet64<IL_OP_GDS_UMAX,
      "_id($id)", atom_r_umax_noret>;
  def ATOM64_R_UMIN_NORET    : BinAtomNoRet64<IL_OP_GDS_UMIN,
      "_id($id)", atom_r_umin_noret>;
  def ATOM64_R_MSKOR_NORET   : TriAtomNoRet64<IL_OP_GDS_MSKOR,
      "_id($id)", atom_r_mskor_noret>;
  def ATOM64_R_OR_NORET      : BinAtomNoRet64<IL_OP_GDS_OR,
      "_id($id)", atom_r_or_noret>;
  def ATOM64_R_RSUB_NORET    : BinAtomNoRet64<IL_OP_GDS_RSUB,
      "_id($id)", atom_r_rsub_noret>;
  def ATOM64_R_SUB_NORET     : BinAtomNoRet64<IL_OP_GDS_SUB,
      "_id($id)", atom_r_sub_noret>;
  def ATOM64_R_XOR_NORET     : BinAtomNoRet64<IL_OP_GDS_XOR,
      "_id($id)", atom_r_xor_noret>;
  def ATOM64_R_INC_NORET     : BinAtomNoRet64<IL_OP_GDS_INC,
      "_id($id)", atom_r_inc_noret>;
  def ATOM64_R_DEC_NORET     : BinAtomNoRet64<IL_OP_GDS_DEC,
      "_id($id)", atom_r_dec_noret>;
  def ATOM64_R_CMPXCHG_NORET    : CmpXChgNoRet64<IL_OP_GDS_CMP,
      "_id($id)", atom_r_cmpxchg_noret>;
  // All of the atomic functions that return
  def ATOM64_G_ADD     : BinAtom64<IL_OP_UAV_READ_ADD,
      "_addr(64)_id($id)", atom_g_add>;
  def ATOM64_G_AND     : BinAtom64<IL_OP_UAV_READ_AND,
      "_addr(64)_id($id)", atom_g_and>;
  def ATOM64_G_MAX     : BinAtom64<IL_OP_UAV_READ_MAX,
      "_addr(64)_id($id)", atom_g_max>;
  def ATOM64_G_MIN     : BinAtom64<IL_OP_UAV_READ_MIN,
      "_addr(64)_id($id)", atom_g_min>;
  def ATOM64_G_UMAX    : BinAtom64<IL_OP_UAV_READ_UMAX,
      "_addr(64)_id($id)", atom_g_umax>;
  def ATOM64_G_UMIN    : BinAtom64<IL_OP_UAV_READ_UMIN,
      "_addr(64)_id($id)", atom_g_umin>;
  def ATOM64_G_OR      : BinAtom64<IL_OP_UAV_READ_OR,
      "_addr(64)_id($id)", atom_g_or>;
  def ATOM64_G_RSUB    : BinAtom64<IL_OP_UAV_READ_RSUB,
      "_addr(64)_id($id)", atom_g_rsub>;
  def ATOM64_G_SUB     : BinAtom64<IL_OP_UAV_READ_SUB,
      "_addr(64)_id($id)", atom_g_sub>;
  def ATOM64_G_XOR     : BinAtom64<IL_OP_UAV_READ_XOR,
      "_addr(64)_id($id)", atom_g_xor>;
  def ATOM64_G_INC     : BinAtom64<IL_OP_UAV_READ_INC,
      "_addr(64)_id($id)", atom_g_inc>;
  def ATOM64_G_DEC     : BinAtom64<IL_OP_UAV_READ_DEC,
      "_addr(64)_id($id)", atom_g_dec>;
  def ATOM64_G_XCHG    : BinAtom64<IL_OP_UAV_READ_XCHG,
      "_addr(64)_id($id)", atom_g_xchg>;
  def ATOM64_G_CMPXCHG : CmpXChg64<IL_OP_UAV_READ_CMPXCHG,
      "_addr(64)_id($id)", atom_g_cmpxchg>;
  def ATOM64_L_ADD     : BinAtom64<IL_OP_LDS_READ_ADD,
      "_id($id)", atom_l_add>;
  def ATOM64_L_AND     : BinAtom64<IL_OP_LDS_READ_AND,
      "_id($id)", atom_l_and>;
  def ATOM64_L_MAX     : BinAtom64<IL_OP_LDS_READ_MAX,
      "_id($id)", atom_l_max>;
  def ATOM64_L_MIN     : BinAtom64<IL_OP_LDS_READ_MIN,
      "_id($id)", atom_l_min>;
  def ATOM64_L_UMAX    : BinAtom64<IL_OP_LDS_READ_UMAX,
      "_id($id)", atom_l_umax>;
  def ATOM64_L_UMIN    : BinAtom64<IL_OP_LDS_READ_UMIN,
      "_id($id)", atom_l_umin>;
  def ATOM64_L_OR      : BinAtom64<IL_OP_LDS_READ_OR,
      "_id($id)", atom_l_or>;
  def ATOM64_L_MSKOR   : TriAtom64<IL_OP_LDS_READ_MSKOR,
      "_id($id)", atom_l_mskor>;
  def ATOM64_L_RSUB    : BinAtom64<IL_OP_LDS_READ_RSUB,
      "_id($id)", atom_l_rsub>;
  def ATOM64_L_SUB     : BinAtom64<IL_OP_LDS_READ_SUB,
      "_id($id)", atom_l_sub>;
  def ATOM64_L_XOR     : BinAtom64<IL_OP_LDS_READ_XOR,
      "_id($id)", atom_l_xor>;
  def ATOM64_L_INC     : BinAtom64<IL_OP_LDS_READ_INC,
      "_id($id)", atom_l_inc>;
  def ATOM64_L_DEC     : BinAtom64<IL_OP_LDS_READ_DEC,
      "_id($id)", atom_l_dec>;
  def ATOM64_L_XCHG    : BinAtom64<IL_OP_LDS_READ_XCHG,
      "_id($id)", atom_l_xchg>;
  def ATOM64_L_CMPXCHG : TriAtom64<IL_OP_LDS_READ_CMPXCHG,
      "_id($id)", atom_l_cmpxchg>;
  def ATOM64_R_ADD     : BinAtom64<IL_OP_GDS_READ_ADD,
      "_id($id)", atom_r_add>;
  def ATOM64_R_AND     : BinAtom64<IL_OP_GDS_READ_AND,
      "_id($id)", atom_r_and>;
  def ATOM64_R_MAX     : BinAtom64<IL_OP_GDS_READ_MAX,
      "_id($id)", atom_r_max>;
  def ATOM64_R_MIN     : BinAtom64<IL_OP_GDS_READ_MIN,
      "_id($id)", atom_r_min>;
  def ATOM64_R_UMAX    : BinAtom64<IL_OP_GDS_READ_UMAX,
      "_id($id)", atom_r_umax>;
  def ATOM64_R_UMIN    : BinAtom64<IL_OP_GDS_READ_UMIN,
      "_id($id)", atom_r_umin>;
  def ATOM64_R_OR      : BinAtom64<IL_OP_GDS_READ_OR,
      "_id($id)", atom_r_or>;
  def ATOM64_R_MSKOR   : TriAtom64<IL_OP_GDS_READ_MSKOR,
      "_id($id)", atom_r_mskor>;
  def ATOM64_R_RSUB    : BinAtom64<IL_OP_GDS_READ_RSUB,
      "_id($id)", atom_r_rsub>;
  def ATOM64_R_SUB     : BinAtom64<IL_OP_GDS_READ_SUB,
      "_id($id)", atom_r_sub>;
  def ATOM64_R_XOR     : BinAtom64<IL_OP_GDS_READ_XOR,
      "_id($id)", atom_r_xor>;
  def ATOM64_R_INC     : BinAtom64<IL_OP_GDS_READ_INC,
      "_id($id)", atom_r_inc>;
  def ATOM64_R_DEC     : BinAtom64<IL_OP_GDS_READ_DEC,
      "_id($id)", atom_r_dec>;
  def ATOM64_R_XCHG    : BinAtom64<IL_OP_GDS_READ_XCHG,
      "_id($id)", atom_r_xchg>;
  def ATOM64_R_CMPXCHG : CmpXChg64<IL_OP_GDS_READ_CMPXCHG,
      "_id($id)", atom_r_cmpxchg>;
  // 64bit atomic operations.
  def ATOM64_G_ADD_NORET_B64  : BinAtomNoRet64I64<IL_OP_UAV_ADD,
      "_b64_addr(64)_id($id)", atom_g_add_noret>;
  def ATOM64_G_AND_NORET_B64  : BinAtomNoRet64I64<IL_OP_UAV_AND,
      "_b64_addr(64)_id($id)", atom_g_and_noret>;
  def ATOM64_G_MAX_NORET_B64  : BinAtomNoRet64I64<IL_OP_UAV_MAX,
      "_b64_addr(64)_id($id)", atom_g_max_noret>;
  def ATOM64_G_MIN_NORET_B64  : BinAtomNoRet64I64<IL_OP_UAV_MIN,
      "_b64_addr(64)_id($id)", atom_g_min_noret>;
  def ATOM64_G_UMAX_NORET_B64 : BinAtomNoRet64I64<IL_OP_UAV_UMAX,
      "_b64_addr(64)_id($id)", atom_g_umax_noret>;
  def ATOM64_G_UMIN_NORET_B64 : BinAtomNoRet64I64<IL_OP_UAV_UMIN,
      "_b64_addr(64)_id($id)", atom_g_umin_noret>;
  def ATOM64_G_OR_NORET_B64   : BinAtomNoRet64I64<IL_OP_UAV_OR,
      "_b64_addr(64)_id($id)", atom_g_or_noret>;
  def ATOM64_G_RSUB_NORET_B64 : BinAtomNoRet64I64<IL_OP_UAV_RSUB,
      "_b64_addr(64)_id($id)", atom_g_rsub_noret>;
  def ATOM64_G_SUB_NORET_B64  : BinAtomNoRet64I64<IL_OP_UAV_SUB,
      "_b64_addr(64)_id($id)", atom_g_sub_noret>;
  def ATOM64_G_XOR_NORET_B64  : BinAtomNoRet64I64<IL_OP_UAV_XOR,
      "_b64_addr(64)_id($id)", atom_g_xor_noret>;
  def ATOM64_G_INC_NORET_B64  : BinAtomNoRet64I64<IL_OP_UAV_INC,
      "_b64_addr(64)_id($id)", atom_g_inc_noret>;
  def ATOM64_G_DEC_NORET_B64  : BinAtomNoRet64I64<IL_OP_UAV_DEC,
      "_b64_addr(64)_id($id)", atom_g_dec_noret>;
  def ATOM64_G_CMPXCHG_NORET_B64 : CmpXChgNoRet64I64<IL_OP_UAV_CMP,
      "_b64_addr(64)_id($id)", atom_g_cmpxchg_noret>;
  def ATOM64_G_ADD_B64     : BinAtom64I64<IL_OP_UAV_READ_ADD,
      "_b64_addr(64)_id($id)", atom_g_add>;
  def ATOM64_G_AND_B64     : BinAtom64I64<IL_OP_UAV_READ_AND,
      "_b64_addr(64)_id($id)", atom_g_and>;
  def ATOM64_G_MAX_B64     : BinAtom64I64<IL_OP_UAV_READ_MAX,
      "_b64_addr(64)_id($id)", atom_g_max>;
  def ATOM64_G_MIN_B64     : BinAtom64I64<IL_OP_UAV_READ_MIN,
      "_b64_addr(64)_id($id)", atom_g_min>;
  def ATOM64_G_UMAX_B64    : BinAtom64I64<IL_OP_UAV_READ_UMAX,
      "_b64_addr(64)_id($id)", atom_g_umax>;
  def ATOM64_G_UMIN_B64    : BinAtom64I64<IL_OP_UAV_READ_UMIN,
      "_b64_addr(64)_id($id)", atom_g_umin>;
  def ATOM64_G_OR_B64      : BinAtom64I64<IL_OP_UAV_READ_OR,
      "_b64_addr(64)_id($id)", atom_g_or>;
  def ATOM64_G_RSUB_B64    : BinAtom64I64<IL_OP_UAV_READ_RSUB,
      "_b64_addr(64)_id($id)", atom_g_rsub>;
  def ATOM64_G_SUB_B64     : BinAtom64I64<IL_OP_UAV_READ_SUB,
      "_b64_addr(64)_id($id)", atom_g_sub>;
  def ATOM64_G_XOR_B64     : BinAtom64I64<IL_OP_UAV_READ_XOR,
      "_b64_addr(64)_id($id)", atom_g_xor>;
  def ATOM64_G_INC_B64     : BinAtom64I64<IL_OP_UAV_READ_INC,
      "_b64_addr(64)_id($id)", atom_g_inc>;
  def ATOM64_G_DEC_B64     : BinAtom64I64<IL_OP_UAV_READ_DEC,
      "_b64_addr(64)_id($id)", atom_g_dec>;
  def ATOM64_G_XCHG_B64    : BinAtom64I64<IL_OP_UAV_READ_XCHG,
      "_b64_addr(64)_id($id)", atom_g_xchg>;
  def ATOM64_G_CMPXCHG_B64 : CmpXChg64I64<IL_OP_UAV_READ_CMPXCHG,
      "_b64_addr(64)_id($id)", atom_g_cmpxchg>;
  def ATOM64_L_ADD_NORET_B64  : BinAtomNoRet64I64<IL_OP_LDS_ADD,
      "64_id($id)", atom_l_add_noret>;
  def ATOM64_L_AND_NORET_B64  : BinAtomNoRet64I64<IL_OP_LDS_AND,
      "64_id($id)", atom_l_and_noret>;
  def ATOM64_L_MAX_NORET_B64  : BinAtomNoRet64I64<IL_OP_LDS_MAX,
      "64_id($id)", atom_l_max_noret>;
  def ATOM64_L_MIN_NORET_B64  : BinAtomNoRet64I64<IL_OP_LDS_MIN,
      "64_id($id)", atom_l_min_noret>;
  def ATOM64_L_UMAX_NORET_B64 : BinAtomNoRet64I64<IL_OP_LDS_UMAX,
      "64_id($id)", atom_l_umax_noret>;
  def ATOM64_L_UMIN_NORET_B64 : BinAtomNoRet64I64<IL_OP_LDS_UMIN,
      "64_id($id)", atom_l_umin_noret>;
  def ATOM64_L_MSKOR_NORET_B64: TriAtomNoRet64I64<IL_OP_LDS_MSKOR,
      "64_id($id)", atom_l_mskor_noret>;
  def ATOM64_L_OR_NORET_B64   : BinAtomNoRet64I64<IL_OP_LDS_OR,
      "64_id($id)", atom_l_or_noret>;
  def ATOM64_L_RSUB_NORET_B64 : BinAtomNoRet64I64<IL_OP_LDS_RSUB,
      "64_id($id)", atom_l_rsub_noret>;
  def ATOM64_L_SUB_NORET_B64  : BinAtomNoRet64I64<IL_OP_LDS_SUB,
      "64_id($id)", atom_l_sub_noret>;
  def ATOM64_L_XOR_NORET_B64  : BinAtomNoRet64I64<IL_OP_LDS_XOR,
      "64_id($id)", atom_l_xor_noret>;
  def ATOM64_L_INC_NORET_B64  : BinAtomNoRet64I64<IL_OP_LDS_INC,
      "64_id($id)", atom_l_inc_noret>;
  def ATOM64_L_DEC_NORET_B64  : BinAtomNoRet64I64<IL_OP_LDS_DEC,
      "64_id($id)", atom_l_dec_noret>;
  def ATOM64_L_CMPXCHG_NORET_B64 : TriAtomNoRet64I64<IL_OP_LDS_CMP,
      "64_id($id)", atom_l_cmpxchg_noret>;
  def ATOM64_R_ADD_NORET_B64  : BinAtomNoRet64I64<IL_OP_GDS_ADD,
      "64_id($id)", atom_r_add_noret>;
  def ATOM64_R_AND_NORET_B64  : BinAtomNoRet64I64<IL_OP_GDS_AND,
      "64_id($id)", atom_r_and_noret>;
  def ATOM64_R_MAX_NORET_B64  : BinAtomNoRet64I64<IL_OP_GDS_MAX,
      "64_id($id)", atom_r_max_noret>;
  def ATOM64_R_MIN_NORET_B64  : BinAtomNoRet64I64<IL_OP_GDS_MIN,
      "64_id($id)", atom_r_min_noret>;
  def ATOM64_R_UMAX_NORET_B64 : BinAtomNoRet64I64<IL_OP_GDS_UMAX,
      "64_id($id)", atom_r_umax_noret>;
  def ATOM64_R_UMIN_NORET_B64 : BinAtomNoRet64I64<IL_OP_GDS_UMIN,
      "64_id($id)", atom_r_umin_noret>;
  def ATOM64_R_MSKOR_NORET_B64: TriAtomNoRet64I64<IL_OP_GDS_MSKOR,
      "64_id($id)", atom_r_mskor_noret>;
  def ATOM64_R_OR_NORET_B64   : BinAtomNoRet64I64<IL_OP_GDS_OR,
      "64_id($id)", atom_r_or_noret>;
  def ATOM64_R_RSUB_NORET_B64 : BinAtomNoRet64I64<IL_OP_GDS_RSUB,
      "64_id($id)", atom_r_rsub_noret>;
  def ATOM64_R_SUB_NORET_B64  : BinAtomNoRet64I64<IL_OP_GDS_SUB,
      "64_id($id)", atom_r_sub_noret>;
  def ATOM64_R_XOR_NORET_B64  : BinAtomNoRet64I64<IL_OP_GDS_XOR,
      "64_id($id)", atom_r_xor_noret>;
  def ATOM64_R_INC_NORET_B64  : BinAtomNoRet64I64<IL_OP_GDS_INC,
      "64_id($id)", atom_r_inc_noret>;
  def ATOM64_R_DEC_NORET_B64  : BinAtomNoRet64I64<IL_OP_GDS_DEC,
      "64_id($id)", atom_r_dec_noret>;
  def ATOM64_R_CMPXCHG_NORET_B64 : CmpXChgNoRet64I64<IL_OP_GDS_CMP,
      "64_id($id)", atom_r_cmpxchg_noret>;
  // All of the atomic functions that return
  def ATOM64_L_ADD_B64     : BinAtom64I64<IL_OP_LDS_READ_ADD,
      "64_id($id)", atom_l_add>;
  def ATOM64_L_AND_B64     : BinAtom64I64<IL_OP_LDS_READ_AND,
      "64_id($id)", atom_l_and>;
  def ATOM64_L_MAX_B64     : BinAtom64I64<IL_OP_LDS_READ_MAX,
      "64_id($id)", atom_l_max>;
  def ATOM64_L_MIN_B64    : BinAtom64I64<IL_OP_LDS_READ_MIN,
      "64_id($id)", atom_l_min>;
  def ATOM64_L_UMAX_B64    : BinAtom64I64<IL_OP_LDS_READ_UMAX,
      "64_id($id)", atom_l_umax>;
  def ATOM64_L_UMIN_B64    : BinAtom64I64<IL_OP_LDS_READ_UMIN,
      "64_id($id)", atom_l_umin>;
  def ATOM64_L_OR_B64      : BinAtom64I64<IL_OP_LDS_READ_OR,
      "64_id($id)", atom_l_or>;
  def ATOM64_L_MSKOR_B64   : TriAtom64I64<IL_OP_LDS_READ_MSKOR,
      "64_id($id)", atom_l_mskor>;
  def ATOM64_L_RSUB_B64    : BinAtom64I64<IL_OP_LDS_READ_RSUB,
      "64_id($id)", atom_l_rsub>;
  def ATOM64_L_SUB_B64     : BinAtom64I64<IL_OP_LDS_READ_SUB,
      "64_id($id)", atom_l_sub>;
  def ATOM64_L_XOR_B64     : BinAtom64I64<IL_OP_LDS_READ_XOR,
      "64_id($id)", atom_l_xor>;
  def ATOM64_L_INC_B64     : BinAtom64I64<IL_OP_LDS_READ_INC,
      "64_id($id)", atom_l_inc>;
  def ATOM64_L_DEC_B64     : BinAtom64I64<IL_OP_LDS_READ_DEC,
      "64_id($id)", atom_l_dec>;
  def ATOM64_L_XCHG_B64    : BinAtom64I64<IL_OP_LDS_READ_XCHG,
      "64_id($id)", atom_l_xchg>;
  def ATOM64_L_CMPXCHG_B64 : TriAtom64I64<IL_OP_LDS_READ_CMPXCHG,
      "64_id($id)", atom_l_cmpxchg>;
  def ATOM64_R_ADD_B64     : BinAtom64I64<IL_OP_GDS_READ_ADD,
      "64_id($id)", atom_r_add>;
  def ATOM64_R_AND_B64     : BinAtom64I64<IL_OP_GDS_READ_AND,
      "64_id($id)", atom_r_and>;
  def ATOM64_R_MAX_B64     : BinAtom64I64<IL_OP_GDS_READ_MAX,
      "64_id($id)", atom_r_max>;
  def ATOM64_R_MIN_B64     : BinAtom64I64<IL_OP_GDS_READ_MIN,
      "64_id($id)", atom_r_min>;
  def ATOM64_R_UMAX_B64    : BinAtom64I64<IL_OP_GDS_READ_UMAX,
      "64_id($id)", atom_r_umax>;
  def ATOM64_R_UMIN_B64    : BinAtom64I64<IL_OP_GDS_READ_UMIN,
      "64_id($id)", atom_r_umin>;
  def ATOM64_R_OR_B64      : BinAtom64I64<IL_OP_GDS_READ_OR,
      "64_id($id)", atom_r_or>;
  def ATOM64_R_MSKOR_B64   : TriAtom64I64<IL_OP_GDS_READ_MSKOR,
      "64_id($id)", atom_r_mskor>;
  def ATOM64_R_RSUB_B64    : BinAtom64I64<IL_OP_GDS_READ_RSUB,
      "64_id($id)", atom_r_rsub>;
  def ATOM64_R_SUB_B64     : BinAtom64I64<IL_OP_GDS_READ_SUB,
      "64_id($id)", atom_r_sub>;
  def ATOM64_R_XOR_B64     : BinAtom64I64<IL_OP_GDS_READ_XOR,
      "64_id($id)", atom_r_xor>;
  def ATOM64_R_INC_B64     : BinAtom64I64<IL_OP_GDS_READ_INC,
      "64_id($id)", atom_r_inc>;
  def ATOM64_R_DEC_B64     : BinAtom64I64<IL_OP_GDS_READ_DEC,
      "64_id($id)", atom_r_dec>;
  def ATOM64_R_XCHG_B64    : BinAtom64I64<IL_OP_GDS_READ_XCHG,
      "64_id($id)", atom_r_xchg>;
  def ATOM64_R_CMPXCHG_B64 : CmpXChg64I64<IL_OP_GDS_READ_CMPXCHG,
      "64_id($id)", atom_r_cmpxchg>;

  // atomic counter operations.
  def APPEND64_ALLOC : Append64<IL_OP_APPEND_BUF_ALLOC,
      "_id($id)", append_alloc>;
  def APPEND64_CONSUME : Append64<IL_OP_APPEND_BUF_CONSUME,
      "_id($id)", append_consume>;
}
