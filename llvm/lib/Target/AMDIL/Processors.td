//===-- Processors.td -----------------------------------------------------===//
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

class Proc<string Name, list<SubtargetFeature> Features>
: Processor<Name, NoItineraries, Features>;
def : Proc<"rv710",      []>;
def : Proc<"rv730",      []>;
def : Proc<"rv770",      [FeatureFP64]>;
def : Proc<"cedar",      [FeatureByteAddress, FeatureImages]>;
def : Proc<"redwood",    [FeatureByteAddress, FeatureImages]>;
def : Proc<"juniper",    [FeatureByteAddress, FeatureImages]>;
def : Proc<"cypress",    [FeatureByteAddress, FeatureImages, FeatureFP64]>;
def : Proc<"barts",      [FeatureByteAddress, FeatureImages]>;
def : Proc<"turks",      [FeatureByteAddress, FeatureImages]>;
def : Proc<"caicos",     [FeatureByteAddress, FeatureImages]>;
def : Proc<"kauai",      [FeatureByteAddress, FeatureImages, FeatureFP64]>;
def : Proc<"cayman",     [FeatureByteAddress, FeatureImages, FeatureFP64]>;
def : Proc<"tahiti",     [FeatureByteAddress, FeatureImages, FeatureFP64]>;
def : Proc<"pitcairn",   [FeatureByteAddress, FeatureImages, FeatureFP64]>;
def : Proc<"capeverde",  [FeatureByteAddress, FeatureImages, FeatureFP64]>;
def : Proc<"trinity",    [FeatureByteAddress, FeatureImages, FeatureFP64]>;
def : Proc<"dogs",       [FeatureByteAddress, FeatureImages, FeatureFP64]>;
def : Proc<"cats",       [FeatureByteAddress, FeatureImages, FeatureFP64]>;

