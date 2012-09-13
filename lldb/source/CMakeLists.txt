add_subdirectory(API)
add_subdirectory(Breakpoint)
add_subdirectory(Commands)
add_subdirectory(Core)
add_subdirectory(Expression)
add_subdirectory(Host)
add_subdirectory(Interpreter)
add_subdirectory(Plugins)
add_subdirectory(Symbol)
add_subdirectory(Target)
add_subdirectory(Utility)

add_lldb_library(lldbInitAndLog
  lldb.cpp
  lldb-log.cpp
  )
