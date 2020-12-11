#ifndef VALUE_TYPE
#error "You need to set VALUE_TYPE to a valid Fortran type"
#endif
#ifndef TYPELETTER
#error "You need to set TYPELETTER to a valid prefix"
#endif

#ifdef __GFORTRAN__
#define _PASTE(a) a
#define CONCAT(a,b)   _PASTE(a)b
#else
#define _PASTE(a)      a ## b
#define CONCAT(a,b)    _PASTE(a,b)
#endif

#define GENERIC(name)  CONCAT(TYPELETTER,name)
