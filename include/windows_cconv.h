/* Taken from Win32. 64-bit Windows uses the 'ccall' calling convention
   instead of 'stdcall'
*/

#ifndef __WINDOWS_CCONV_H
#define __WINDOWS_CCONV_H

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

#endif
