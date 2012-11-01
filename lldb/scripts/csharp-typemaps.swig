%define %cs_marshal_intptr(TYPE, ARGNAME...) 
        %typemap(ctype)  TYPE ARGNAME "void*" 
        %typemap(imtype) TYPE ARGNAME "IntPtr" 
        %typemap(cstype) TYPE ARGNAME "IntPtr" 
        %typemap(in)     TYPE ARGNAME %{ $1 = ($1_ltype)$input; /* IntPtr */ %} 
        %typemap(csin)   TYPE ARGNAME "$csinput" 
        
        %typemap(out)    TYPE ARGNAME %{ $result = $1; %} 
        %typemap(csout, excode=SWIGEXCODE) TYPE ARGNAME { 
                IntPtr cPtr = $imcall;$excode 
                return cPtr; 
        } 
        %typemap(csvarout, excode=SWIGEXCODE2) TYPE ARGNAME %{ 
                get { 
                        IntPtr cPtr = $imcall;$excode 
                        return cPtr; 
                } 
        %} 

        %typemap(ctype)  TYPE& ARGNAME "void**" 
        %typemap(imtype) TYPE& ARGNAME "ref IntPtr" 
        %typemap(cstype) TYPE& ARGNAME  "ref IntPtr" 
        %typemap(in)     TYPE& ARGNAME %{ $1 = ($1_ltype)$input; %} 
        %typemap(csin)   TYPE& ARGNAME "ref $csinput" 
%enddef 

%INTEGRAL_ARRAY_TYPEMAP(unsigned int,         bool); 
%INTEGRAL_ARRAY_TYPEMAP(char,                 char); 
%INTEGRAL_ARRAY_TYPEMAP(signed char,          sbyte); 
%INTEGRAL_ARRAY_TYPEMAP(unsigned char,        byte); 
%INTEGRAL_ARRAY_TYPEMAP(short,                short); 
%INTEGRAL_ARRAY_TYPEMAP(unsigned short,       ushort); 
%INTEGRAL_ARRAY_TYPEMAP(int,                  int); 
%INTEGRAL_ARRAY_TYPEMAP(unsigned int,         uint); 
%INTEGRAL_ARRAY_TYPEMAP(long,                 int); 
%INTEGRAL_ARRAY_TYPEMAP(unsigned long,        uint); 
%INTEGRAL_ARRAY_TYPEMAP(long long,            long); 
%INTEGRAL_ARRAY_TYPEMAP(unsigned long long,   ulong); 
%INTEGRAL_ARRAY_TYPEMAP(float,                float); 
%INTEGRAL_ARRAY_TYPEMAP(double,               double);



%INTEGRAL_ARRAY_TYPEMAP(int8_t,          sbyte); 
%INTEGRAL_ARRAY_TYPEMAP(uint8_t,        byte); 
%INTEGRAL_ARRAY_TYPEMAP(short,                short); 
%INTEGRAL_ARRAY_TYPEMAP(unsigned short,       ushort); 
%INTEGRAL_ARRAY_TYPEMAP(int,                  int); 
%INTEGRAL_ARRAY_TYPEMAP(unsigned int,         uint); 
%INTEGRAL_ARRAY_TYPEMAP(long,                 int); 
%INTEGRAL_ARRAY_TYPEMAP(unsigned long,        uint); 
%INTEGRAL_ARRAY_TYPEMAP(long long,            long); 
%INTEGRAL_ARRAY_TYPEMAP(unsigned long long,   ulong); 
%INTEGRAL_ARRAY_TYPEMAP(float,                float); 
%INTEGRAL_ARRAY_TYPEMAP(double,               double);

%cs_marshal_intptr(void*) 