// RUN: clang-cc -verify -fsyntax-only -std=c90 %s &&
// RUN: clang-cc -verify -fsyntax-only -std=c99 %s

int f (int x)
{
  // sizeof applied to a type should not delete the type.
  return sizeof (int[x]);
}
