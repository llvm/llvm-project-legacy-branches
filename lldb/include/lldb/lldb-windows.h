//===-- lldb-windows.h ------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef lldb_lldb_windows_h_
#define lldb_lldb_windows_h_

#if defined(__cplusplus)

#ifdef _MSC_VER

#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>

// posix utilities
#define snprintf _snprintf
int vasprintf(char **ret, const char *fmt, va_list ap);

int strcasecmp(const char* s1, const char* s2);
int strncasecmp(const char* s1, const char* s2, size_t n);
char * strcasestr(const char *s, const char* find);

long long int strtoll(const char *nptr, char **endptr, int base);
unsigned long long int strtoull(const char *nptr, char **endptr, int base);

// missing posix declarations
enum {
  PATH_MAX = MAX_PATH
};

/*
enum {
  REG_EXTENDED
};*/

enum {
  STDIN_FILENO = 0,
  STDOUT_FILENO,
  STDERR_FILENO
};

enum {
  O_NOCTTY
};

enum {
  SIGKILL
};

// missing socket types
typedef short sa_family_t;
typedef unsigned short in_port_t;

// getopt()
enum {
  no_argument = 0,
  required_argument,
  optional_argument
};

struct option {
    /* name of long option */
    const char *name;
    /*
     * one of no_argument, required_argument, and optional_argument:
     * whether option takes an argument
     */
    int has_arg;
    /* if not NULL, set *flag to val when option found */
    int *flag;
    /* if flag not NULL, value to set *flag to; else return value */
    int val;
};

int getopt(int argc, char * const argv[],
    const char *optstring);

extern char *optarg;
extern int optind, opterr, optopt;

int getopt_long(int argc, char * const *argv,
    const char *optstring,
    const struct option *longopts, int *longindex);

int getopt_long_only(int argc, char * const *argv,
    const char *optstring,
    const struct option *longopts, int *longindex);
extern int optreset;

// missing macros 
#define __PRETTY_FUNCTION__ __FUNCSIG__
#define va_copy(d,s) ((d) = (s))

#endif

#define PRId32        "ld"
#define PRIi32        "li"
#define PRIo32        "lo"
#define PRIu32        "lu"
#define PRIx32        "lx"
#define PRIX32        "lX"
#ifndef PRId64
#define PRId64        "lld"
#endif
#ifndef PRIi64
#define PRIi64        "lli"
#endif
#ifndef PRIo64
#define PRIo64        "llo"
#endif
#ifndef PRIu64
#define PRIu64        "llu"
#endif
#ifndef PRIx64
#define PRIx64        "llx"
#endif
#ifndef PRIX64
#define PRIX64        "llX"
#endif

#define __WALL 0x40000000


#define	SIGHUP	1	
#define	SIGINT	2	
#define	SIGQUIT	3	
#define	SIGILL	4	
#define	SIGTRAP	5	
#define SIGIOT  6
#define	SIGEMT	7	
#define	SIGFPE	8	
#define	SIGKILL	9	
#define	SIGBUS	10	
#define	SIGSEGV	11	
#define	SIGSYS	12	
#define	SIGPIPE	13	
#define	SIGALRM	14	
#define	SIGTERM	15	
#define	SIGURG	16	
#define SIGSTKFLT 16
#define	SIGSTOP	17	
#define	SIGTSTP	18	
#define	SIGCONT	19	
#define	SIGCHLD	20	
#define	SIGTTIN	21	
#define	SIGTTOU	22	
#define	SIGIO	23	
#define SIGPOLL 23
#define	SIGXCPU	24	
#define	SIGXFSZ	25	
#define	SIGVTALRM 26	
#define	SIGPROF	27	
#define SIGWINCH 28	
#define SIGINFO	29	
#define SIGUSR1 30	
#define SIGPWR 30
#define SIGUSR2 31	



#endif  // defined(__cplusplus)


#endif  // lldb_lldb_windows_h_
