add_clang_unittest(DynamicASTMatchersTests
  GenericMatcherTest.cpp
  GenericValueTest.cpp
  NumberHolderTest.cpp
  ParserTest.cpp
  RegistryTest.cpp)

target_link_libraries(DynamicASTMatchersTests
  gtest gtest_main clangASTMatchers clangDynamicASTMatchers clangTooling)
