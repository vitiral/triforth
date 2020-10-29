# Typeforth Differences with ANS Forth

The primary high level differences are:
- supports modules (namespace), sumtypes (variadic enums), prodtypes (structs) and vtables (interfaces)
- naming is more modern and consistent. I.e. `.stack` prints the stack and
  `.ix` prints an integer as hex.  All "print" operations are of the form
  `.XXX`. Also, using "type" is out of the question as it normally refers to a
  data-type so would be extremely confusing.
- More compatible across architectures -- most types are not dependent on the
  cell size.
- Supports different sizes for code and data (i.e. 16 bit code pointer, 32 bit
  data pointer), enabling more compact code if there is less that 64k of code.

Description                     | TF Type     | ANS Type        | Reason
---------------------           | ----------- | ----------      | ------
unsigned 8 bits of data         | b, byte     | c, char         |
8 bit ascii value               | ascii       | c, char         | TF "char" reserved for unicode
signed 8 bits of data           | sb, sbyte   | ?               | 
unsigned 16 bits of data        | i, int      | u iff 16bit cell, else ? | in TF "i" always representes 16 bit integer
signed 16 bits of data          | si, sint    | i iff 16bit cell, else ? |
unsigned 32 bits of data        | d, double   | d if 16 bit cell else u if 32 bit cell | in TF "d" always represents 32 bit integer
signed 32 bits of data          | sd, sdouble | i if 32 bit cell else ?  |
data address (pointer)          | &           | addr            | 
byte address                    | &b          | c-addr          | 
8 bit data access               | b@          | c@              | 
16 bit data access              | i@          | @ if 16 bit cell else ? |
32 bit data access              | d@          | @ if 32 bit cell else ? |
executable code                 | x           | code point      |
executable code address         | &x          | xt              | Note: also called an "execution token". TF supports different code/data address sizes
code address access             | x@          | @               | 
module/method/vtable access     | `.`         | N/A             | TF supports modules, methods and vtables
( byte-address count ) string   | s           | ( c-addr count ) | TF has specific name for strings
print string to stderr          | `.s`        | `type`          |
print signed 16 bit integer as hex | `.six`   | `.` if HEX and 16 bit cell | Note: TF doesn't append spaces like ANS does.
print unsigned 16 bit integer as hex | `.ix`  | `u.` if HEX and 16 bit cell | ANS inconsistent with where the `.` goes, TF always has `.` first
print stack to stderr           | `.stack`    | `.s` if HEX and 16 bit cell |
string literal                  | `\" ...\"`  | `\" ..."`       | TF uses `\"` for both opening and closing, allowing nested `"`
formatted string literal        | `\" .. $word \"`| ??          | TF allows string formatting with arbitrary embedded expressions.


