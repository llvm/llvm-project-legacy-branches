#include "lldb/Host/ReadWriteLock.h"

namespace lldb_private {

// Windows has slim read-writer lock support on Vista and higher, so we
// will attempt to load the APIs.  If they exist, we will use them, and
// if not, we will fall back on critical sections.  When we drop support
// for XP, we can stop lazy-loading these APIs and just use them directly.
#if defined(__MINGW32__)
  // Taken from WinNT.h
  typedef struct _RTL_SRWLOCK {
    PVOID Ptr;
  } RTL_SRWLOCK, *PRTL_SRWLOCK;

  // Taken from WinBase.h
  typedef RTL_SRWLOCK SRWLOCK, *PSRWLOCK;
#endif


typedef struct Win32RWLOCK {
        unsigned long int readlockcount;
        HANDLE writable;
        CRITICAL_SECTION writelock;
        unsigned long int writelocked;
} Win32RWLOCK;

typedef Win32RWLOCK* PWin32RWLOCK; 

static VOID (WINAPI *fpInitializeSRWLock)(PSRWLOCK lock) = NULL;
static VOID (WINAPI *fpAcquireSRWLockExclusive)(PSRWLOCK lock) = NULL;
static VOID (WINAPI *fpAcquireSRWLockShared)(PSRWLOCK lock) = NULL;
static VOID (WINAPI *fpReleaseSRWLockExclusive)(PSRWLOCK lock) = NULL;
static VOID (WINAPI *fpReleaseSRWLockShared)(PSRWLOCK lock) = NULL;
static BOOL (WINAPI *fpTryAcquireSRWLockExclusive)(PSRWLOCK lock) = NULL;
static BOOL (WINAPI *fpTryAcquireSRWLockShared)(PSRWLOCK lock) = NULL;

static bool sHasSRW = false;

static bool loadSRW() {
  static bool sChecked = false;
  if (!sChecked) {
    sChecked = true;
    return false;

    HMODULE hLib = ::LoadLibrary(TEXT("Kernel32"));
    if (hLib) {
      fpInitializeSRWLock =
        (VOID (WINAPI *)(PSRWLOCK))::GetProcAddress(hLib,
                                               "InitializeSRWLock");
      fpAcquireSRWLockExclusive =
        (VOID (WINAPI *)(PSRWLOCK))::GetProcAddress(hLib,
                                               "AcquireSRWLockExclusive");
      fpAcquireSRWLockShared =
        (VOID (WINAPI *)(PSRWLOCK))::GetProcAddress(hLib,
                                               "AcquireSRWLockShared");
      fpReleaseSRWLockExclusive =
        (VOID (WINAPI *)(PSRWLOCK))::GetProcAddress(hLib,
                                               "ReleaseSRWLockExclusive");
      fpReleaseSRWLockShared =
        (VOID (WINAPI *)(PSRWLOCK))::GetProcAddress(hLib,
                                               "ReleaseSRWLockShared");
      fpTryAcquireSRWLockExclusive =
        (BOOL (WINAPI *)(PSRWLOCK))::GetProcAddress(hLib,
                                               "TryAcquireSRWLockExclusive");
      fpTryAcquireSRWLockShared =
        (BOOL (WINAPI *)(PSRWLOCK))::GetProcAddress(hLib,
                                               "TryAcquireSRWLockShared");      
          
      ::FreeLibrary(hLib);

      if (fpInitializeSRWLock != NULL) {
        sHasSRW = true;
      }
    }
  }
  return sHasSRW;
}

ReadWriteLock::ReadWriteLock () {
    if (loadSRW()) {
        m_data = calloc(1, sizeof(SRWLOCK));
        fpInitializeSRWLock(static_cast<PSRWLOCK>(m_data));
    } else {
        m_data = calloc(1, sizeof(Win32RWLOCK));
        static_cast<PWin32RWLOCK>(m_data)->readlockcount = 0;
        static_cast<PWin32RWLOCK>(m_data)->writable = CreateEvent(NULL, true, true, NULL);
        InitializeCriticalSection(&static_cast<PWin32RWLOCK>(m_data)->writelock);
    }
}

ReadWriteLock::~ReadWriteLock () {
    if (!sHasSRW) {
        CloseHandle(static_cast<PWin32RWLOCK>(m_data)->writable);
        DeleteCriticalSection(&static_cast<PWin32RWLOCK>(m_data)->writelock);
    }
    free(m_data);
}

bool ReadWriteLock::ReadLock () {
    if (sHasSRW) {
        fpAcquireSRWLockShared(static_cast<PSRWLOCK>(m_data));
        return true;
    } else {
        EnterCriticalSection(&static_cast<PWin32RWLOCK>(m_data)->writelock);
        InterlockedIncrement(&static_cast<PWin32RWLOCK>(m_data)->readlockcount);
        ResetEvent(static_cast<PWin32RWLOCK>(m_data)->writable);
        LeaveCriticalSection(&static_cast<PWin32RWLOCK>(m_data)->writelock);
        return true;
    }
}

bool ReadWriteLock::ReadTryLock () {
    if (sHasSRW) {
        return fpTryAcquireSRWLockShared(static_cast<PSRWLOCK>(m_data)) != 0;
    } else {
        if (TryEnterCriticalSection(&static_cast<PWin32RWLOCK>(m_data)->writelock)) {
            InterlockedIncrement(&static_cast<PWin32RWLOCK>(m_data)->readlockcount);
            ResetEvent(static_cast<PWin32RWLOCK>(m_data)->writable);
            LeaveCriticalSection(&static_cast<PWin32RWLOCK>(m_data)->writelock);
            return true;
        }
        return false;
    }
}

bool ReadWriteLock::ReadUnlock () {
    if (sHasSRW) {
            fpReleaseSRWLockShared(static_cast<PSRWLOCK>(m_data));
        return true;
    } else {
        unsigned long int value = InterlockedDecrement(&static_cast<PWin32RWLOCK>(m_data)->readlockcount);
        assert(((int)value) >= 0);
        if (value == 0)
            SetEvent(static_cast<PWin32RWLOCK>(m_data)->writable);
        return true;
    }
}

bool ReadWriteLock::WriteLock () {
    if (sHasSRW) {
        fpAcquireSRWLockExclusive(static_cast<PSRWLOCK>(m_data));
        return true;
    } else {
        EnterCriticalSection(&static_cast<PWin32RWLOCK>(m_data)->writelock);
        WaitForSingleObject(static_cast<PWin32RWLOCK>(m_data)->writable, INFINITE);
        int res = InterlockedExchange(&static_cast<PWin32RWLOCK>(m_data)->writelocked, 1);
        assert(res == 0);
        return true;
    }
}

bool ReadWriteLock::WriteTryLock () {
    if (sHasSRW) {
        return fpTryAcquireSRWLockExclusive(static_cast<PSRWLOCK>(m_data)) != 0;
    } else {
        if (TryEnterCriticalSection(&static_cast<PWin32RWLOCK>(m_data)->writelock)) {
            if (WaitForSingleObject(static_cast<PWin32RWLOCK>(m_data)->writable, 0)) {
                LeaveCriticalSection(&static_cast<PWin32RWLOCK>(m_data)->writelock);
                return false;
            }
            int res = InterlockedExchange(&static_cast<PWin32RWLOCK>(m_data)->writelocked, 1);
            assert(res == 0);
            return true;
        }
        return false;
    }
}

bool ReadWriteLock::WriteUnlock () {
    if (sHasSRW) {
        fpReleaseSRWLockExclusive(static_cast<PSRWLOCK>(m_data));
        return true;
    } else {
        int res = InterlockedExchange(&static_cast<PWin32RWLOCK>(m_data)->writelocked, 0);
        if (res == 1) {
            LeaveCriticalSection(&static_cast<PWin32RWLOCK>(m_data)->writelock);
            return true;
        }
        return false;
    }
}



}
