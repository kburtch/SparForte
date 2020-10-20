#!/usr/local/bin/spar
trace true;

-- This is a comment

-- THIS IS A BASIC SYNTAX TEST FOR SPARFOTE
-- IT SHOULD RUN THIS WITHOUT STOPPING WITH AN ERROR
-- Run this script with the parameters a, b and c

-- universal types

un : universal_numeric;
us : universal_string;
ut : universal_typeless;

	un1 : universal_numeric; -- tab character
  un2 : universal_numeric; -- spaces
-- other predefined types

i   : integer;
si  : short_integer;
ssi : short_short_integer;
li  : long_integer;
lli : long_long_integer;
f   : float;
lfv : long_float;
sf  : short_float;
dur : duration;
p   : positive;
n   : natural;
c   : character;
c1  : character;
b   : boolean;
s   : string;
s1  : string;
fm  : file_mode;
j,k : integer;
js  : json_string;
ft  : file_type;
unbs : unbounded_string;
-- st will be reported as an unused variable.  Unfortunately, there's no way
-- to use it without opening a socket because it is limited.
-- st  : socket_type;

-- assignment during declaration (syntax only)

i2  : integer := 1;
s2  : constant string := "test";
pragma assumption( factor, s2 );

-- new types, enumerateds and subtypes

subtype sub_int is integer;
pragma assumption( applied, sub_int );
type new_int is new integer;
pragma assumption( applied, new_int );
type new_enum is (enum0);
type new_enum2 is (enum1, enum2, enum3);
type abstract_int is new abstract integer;
subtype abstract_int2 is abstract integer;
limit_int1 : limited integer;
limit_int2 : limited integer := 5;
pragma assumption( used, limit_int1 );
pragma assumption( used, limit_int2 );
type limited_int is new limited integer;
pragma assumption( applied, limited_int );
subtype limited_int2 is limited integer;
pragma assumption( applied, limited_int2 );
-- It is permissible to cast temporarily to an abstract.

declare
  type i1 is new abstract integer;
  type i2 is new i1;
  i : i2;
begin
  i := i2( i1( 1 ) );
end;

-- assignment and expressions

pragma assert( 1=1 );
i := 0;
pragma assert( i=0 );
i := 1000000;
pragma assert( i=1000000 );
i := 1_000_000;
pragma assert( i=1000000 );
f := 0.0;
pragma assert( f=0.0 );
i := +1;
pragma assert( i=1 );
i := -1;
pragma assert( i=-1 );
i := 1 + 1;
pragma assert( i=2 );
i := 1 * 1;
pragma assert( i=1 );
i := 1 ** 1;
pragma assert( i=1 );
i := 1 / 1;
pragma assert( i=1 );
i := 1 - 1;
pragma assert( i=0 );
i := (1 + 1 ) / 1;
pragma assert( i=2 );
i := ((1 + 1 )) / 1;
pragma assert( i=2 );
i:=(1+1)/2;
pragma assert( i=1 );
i := 3 mod 2;
pragma assert( i=1 );
f := 3 rem 2;
pragma assert( f=1 );
i := System.Max_Int;
pragma assert( i=System.Max_Int );
i := System.Max_Int+0;
pragma assert( i=System.Max_Int );
i := System.Max_Int-0;
pragma assert( i=System.Max_Int );
i := System.Max_Int*1;
pragma assert( i=System.Max_Int );
i := System.Max_Int/1;
pragma assert( i=System.Max_Int );
i := System.Max_Int-1+1;
pragma assert( i=System.Max_Int );
i := System.Min_Int;
pragma assert( i=System.Min_Int );
i := System.Min_Int+0;
pragma assert( i=System.Min_Int );
i := System.Min_Int-0;
pragma assert( i=System.Min_Int );
i := System.Min_Int*1;
pragma assert( i=System.Min_Int );
i := System.Min_Int/1;
pragma assert( i=System.Min_Int );
i := System.Min_Int+1-1;
pragma assert( i=System.Min_Int );
s := "'";
pragma assert( s="'" );
s := '"';
pragma assert( s='"' );
s := "string1" & "string2";
pragma assert( s="string1string2" );
s := "string1";
c := 'a';
s := s & c;
pragma assert( s="string1a" );
s := "string1";
c := 'a';
s := c & s;
pragma assert( s="astring1" );
c := 'a';
c1:= 'b';
s := c&c1;
pragma assert( s="ab" );
c := 'a';
s := "unistring"&c;
pragma assert( s="unistringa" );
s := 3 * "str";
pragma assert( s="strstrstr" );
b := true;
pragma assert( b=true );
b := false;
pragma assert( b=false );
b := not true;
pragma assert( b=false );
b := not false;
pragma assert( b=true );
b := true and true;
pragma assert( b=true );
b := true and false;
pragma assert( b=false );
b := false and true;
pragma assert( b=false );
b := false and false;
pragma assert( b=false );
b := true and false and true;
pragma assert( b=false );
b := false and true and false;
pragma assert( b=false );
b := true and true and true;
pragma assert( b=true );
b := false and false and false;
pragma assert( b=false );
b := true or true;
pragma assert( b=true );
b := true or false;
pragma assert( b=true );
b := false or true;
pragma assert( b=true );
b := false or false;
pragma assert( b=false );
b := true or false or true;
pragma assert( b=true );
b := false or true or false;
pragma assert( b=true );
b := true or true or true;
pragma assert( b=true );
b := false or false or false;
pragma assert( b=false );
b := true xor true;
pragma assert( b=false );
b := true xor false;
pragma assert( b=true );
b := false xor true;
pragma assert( b=true );
b := false xor false;
pragma assert( b=false );
b := 1 > 1;
pragma assert( b=false );
b := 1 = 1;
pragma assert( b=true );
b := 1 < 1;
pragma assert( b=false );
b := 1 >= 1;
pragma assert( b=true );
b := 1 <= 1;
pragma assert( b=true );
b := 1 = 1;
pragma assert( b=true );
b := 1 /= 1;
pragma assert( b=false );
b := "1" > "1";
pragma assert( b=false );
b := "1" = "1";
pragma assert( b=true );
b := "1" < "1";
pragma assert( b=false );
b := "1" >= "1";
pragma assert( b=true );
b := "1" <= "1";
pragma assert( b=true );
b := "1" = "1";
pragma assert( b=true );
b := "1" /= "1";
pragma assert( b=false );
i := 0;
i := @ + 1;
pragma assert( i=1 );
i := integer(1);
pragma assert( i=1 );
si := short_integer(1);
pragma assert( si=1 );
ssi := short_short_integer(1);
pragma assert( ssi=1 );
li := long_integer(1);
pragma assert( li=1 );
lli := long_long_integer(1);
pragma assert( lli=1 );
f := float(i);
pragma assert( f=1 );
sf := short_float(i);
pragma assert( sf=1 );
lfv := long_float(i);
pragma assert( lfv=1 );
dur := duration(i);
pragma assert( dur=1 );
p := positive(i);
pragma assert( p=1 );
n := natural(i);
pragma assert( n=1 );
c := character( 'c' );
pragma assert( c='c' );
c := character( "c" );
pragma assert( c="c" );
s := string('c');
pragma assert( s='c' );
s := string("hello");
pragma assert( s="hello" );
i := integer(1+1);
pragma assert( i=2 );
i := 32 or 64;
pragma assert( i=96 );
i := 255 and 32;
pragma assert( i=32 );
j := 32;
i := j or 64;
pragma assert( i=96 );
j := 255;
i := j and 32;
pragma assert( i=32 );
j := 64;
i := 32 or j;
pragma assert( i=96 );
j := 32;
i := 255 and j;
pragma assert( i=32 );
j := 14;
i := j xor 9;
pragma assert( i=7 );
i := 5;
pragma assert( i in 1..10 );
pragma assert( i in i..i );
pragma assert( i not in 10..20 );
pragma assert( i not in 1..4 );
pragma assert( 'c' in 'a'..'z' );
pragma assert( 'c' not in 'a'..'b' );
inenum : new_enum2 := enum1;
pragma assert( inenum in enum1..enum3 );
pragma assert( inenum in inenum..inenum );
pragma assert( inenum not in enum2..enum3 );
inenum := enum3;
pragma assert( inenum not in enum1..enum2 );
un1 := 29;
pragma assert( un1=29 );
un2 := 29.5;
pragma assert( un2=29.5 );
un := un2;
pragma assert( un=29.5 );
us := 'c';
pragma assert( us='c' );
us := "cx";
pragma assert( us="cx" );
ut := 92;
pragma assert( ut=92 );
ut := "jh";
pragma assert( ut="jh" );

-- boolean true shortcuts

bool_true_shortcut : boolean := false;
bool_true_shortcut;
pragma assert( bool_true_shortcut );

subtype shortcut_bool is boolean;
bool_true_shortcut2 : shortcut_bool := false;
bool_true_shortcut2;
pragma assert( bool_true_shortcut2 );

c := ASCII.NUL;
c := ASCII.SOH;
c := ASCII.STX;
c := ASCII.ETX;
c := ASCII.EOT;
c := ASCII.ENQ;
c := ASCII.ACK;
c := ASCII.BEL;
c := ASCII.BS;
c := ASCII.HT;
c := ASCII.LF;
c := ASCII.VT;
c := ASCII.FF;
c := ASCII.CR;
c := ASCII.SO;
c := ASCII.SI;
c := ASCII.DLE;
c := ASCII.DC1;
c := ASCII.DC2;
c := ASCII.DC3;
c := ASCII.DC4;
c := ASCII.NAK;
c := ASCII.SYN;
c := ASCII.ETB;
c := ASCII.CAN;
c := ASCII.EM;
c := ASCII.SUB;
c := ASCII.ESC;
c := ASCII.FS;
c := ASCII.GS;
c := ASCII.RS;
c := ASCII.US;
c := ASCII.DEL;
c := ASCII.Exclam;
c := ASCII.Quotation;
c := ASCII.Sharp;
c := ASCII.Dollar;
c := ASCII.Percent;
c := ASCII.Ampersand;
c := ASCII.Colon;
c := ASCII.Semicolon;
c := ASCII.Query;
c := ASCII.At_Sign;
c := ASCII.L_Bracket;
c := ASCII.Back_Slash;
c := ASCII.R_Bracket;
c := ASCII.Circumflex;
c := ASCII.Underline;
c := ASCII.Grave;
c := ASCII.L_Brace;
c := ASCII.Bar;
c := ASCII.R_Brace;
c := ASCII.Tilde;
us:= System.System_Name;
un:= System.Min_Int;
un:= System.Max_Int;
un:= System.Max_Binary_Modulus;
un:= System.Max_Nonbinary_Modulus;
un:= System.Max_Base_Digits;
un:= System.Max_Digits;
un:= System.Max_Mantissa;
un:= System.Fine_Delta;
un:= System.Tick;
un:= System.Storage_Unit;
un:= System.Word_Size;
un:= System.Memory_Size;
us:= System.Default_Bit_Order;
fm := in_file;
fm := out_file;
fm := append_file;

-- numerics functions

f := numerics.sqrt( 9 );
pragma assert( f = 3 );
p := 9;
f := numerics.sqrt( p );
pragma assert( f = 3 );
n := 9;
f := numerics.sqrt( n );
pragma assert( f = 3 );
f := numerics.sqrt( 9 );
pragma assert( f = 3 );
f := numerics.log( 1.0 );
pragma assert( f = 0 );
f := numerics.log( 1.0, 2.0 );
pragma assert( f = 0 );
f := numerics.exp( 0 );
pragma assert( f = 1 );
f := numerics.random;
pragma assert( f >= 0 and f <= 1 );
i := numerics.shift_left( 8, 1 );
pragma assert( i = 16 );
p := 1;
i := numerics.shift_left( 8, p );
pragma assert( i = 16 );
n := 1;
i := numerics.shift_left( 8, n );
pragma assert( i = 16 );
i  := numerics.rotate_left( 8, 1 );
pragma assert( i = 16 );
i  := numerics.shift_right( 8, 1 );
pragma assert( i = 4 );
i  := numerics.rotate_right( 8, 1 );
pragma assert( i = 4 );
i  := numerics.shift_right_arithmetic( 8, 1 );
pragma assert( i = 4 );
f  := numerics.sin( 0 );
pragma assert( f = 0 );
f  := numerics.sin( 0, 360 );
pragma assert( f = 0 );
f  := numerics.cos( 0 );
pragma assert( f = 1 );
f  := numerics.cos( 0, 360 );
pragma assert( f = 1 );
f  := numerics.tan( 0 );
pragma assert( f = 0 );
f  := numerics.tan( 0, 360 );
pragma assert( f = 0 );
f  := numerics.cot( 1 );
f  := numerics.cot( 1, 360 );
f  := numerics.arcsin( 0 );
f  := numerics.arcsin( 0, 360 );
f  := numerics.arccos( 0 );
f  := numerics.arccos( 0, 360 );
f  := numerics.arctan( 0, 1 );
f  := numerics.arctan( 0, 1, 360 );
f  := numerics.arccot( 0, 1 );
f  := numerics.arccot( 0, 1, 360 );
f  := numerics.sinh( 0 );
pragma assert( f = 0 );
f  := numerics.cosh( 0 );
pragma assert( f = 1 );
f  := numerics.tanh( 0 );
pragma assert( f = 0 );
f  := numerics.coth( 1 );
f  := numerics.arcsinh( 0 );
f  := numerics.arccosh( 1 );
f  := numerics.arctanh( 0 );
f  := numerics.arccoth( 24.5 );
f  := numerics.floor( 24.5 );
pragma assert( f = 24 );
f  := numerics.ceiling( 24.5 );
pragma assert( f = 25 );
f  := numerics.rounding( 24.5 );
pragma assert( f = 25 );
f  := numerics.unbiased_rounding( 24.5 );
pragma assert( f = 24 );
f  := numerics.truncation( 24.5 );
pragma assert( f = 24 );
f  := numerics.remainder( 24.5, 1.0 );
pragma assert( f = 0.5 );
f  := numerics.copy_sign( -1.5, 1.0 );
pragma assert( f = 1.5 );
f  := numerics.copy_sign( -1.5, -1.0 );
pragma assert( f = -1.5 );
f  := numerics.exponent( -1.5 );
pragma assert( f = 1 );
f  := numerics.fraction( 0 );
pragma assert( f = 0 );
f  := numerics.leading_part( -1.5, 1 );
pragma assert( f = -1 );
f  := numerics.min( 1, 2 );
pragma assert( f = 1 );
f  := numerics.min( 2, 1 );
pragma assert( f = 1 );
f  := numerics.max( 1, 2 );
pragma assert( f = 2 );
f  := numerics.max( 2, 1 );
pragma assert( f = 2 );
f  := numerics.machine( 1.5 );
pragma assert( f = 1.5 );
f  := numerics.scaling( 1.5, 1 );
pragma assert( f = 3 );
f  := numerics.scaling( 1.5, 0 );
pragma assert( f = 1.5 );
f  := abs( 1.5 );
pragma assert( f = 1.5 );
f  := abs( -1.5 );
pragma assert( f = 1.5 );
p  := numerics.pos( 'A' );
pragma assert( p = 65 );
i  := numerics.value( "65" );
pragma assert( i = 65 );
p  := numerics.rnd( 5 );
pragma assert( p > 0 and p < 6 );
b := numerics.odd( 1 );
pragma assert( b );
b := numerics.odd( 2 );
pragma assert( not b );
b := numerics.even( 1 );
pragma assert( not b );
b := numerics.even( 2 );
pragma assert( b );
n  := numerics.serial;
pragma assert( n = 0 );
n  := numerics.serial;
pragma assert( n = 1 );
n := numerics.hash_of( "apple", 100);
pragma assert( n = 94 );
n := numerics.fnv_hash_of( "apple", 100);
pragma assert( n = 29 );
n := numerics.murmur_hash_of( "apple", 100);
pragma assert( n = 96 );
n := numerics.sdbm_hash_of( "apple", 100);
pragma assert( n = 59 );
s := numerics.md5( "abc" );
pragma assert( s = "900150983cd24fb0d6963f7d28e17f72" );
s := numerics.sha1_digest_of( "abc" );
pragma assert( s = "a9993e364706816aba3e25717850c26c9cd0d89d" );
s := numerics.sha224_digest_of( "abc" );
pragma assert( s = "23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7" );
s := numerics.sha256_digest_of( "abc" );
pragma assert( s = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" );
s := numerics.sha512_digest_of( "abc" );
pragma assert( s = "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f" );

-- match and glob functions

b := strings.match( "hello", "hello" );
pragma assert( b = true );
b := strings.match( "ht", "hello" );
pragma assert( b = false );
b  := strings.match( "^h", "hello" );
pragma assert( b = true );
b  := strings.glob( "h*", "hello" );
pragma assert( b = true );
b  := strings.glob( "i*", "hello" );
pragma assert( b = false );
b := strings.perl_match( "hello", "hello" );
pragma assert( b = true );
b := strings.perl_match( "ht", "hello" );
pragma assert( b = false );

-- strings functions
--
-- Use variables to verify types

test : string := "test";
n := 1;
p := 1;
c := strings.element( test, p );
pragma assert( c = 't' );
s := strings.slice( test, p, n );
pragma assert( s = "t" );
n := strings.index( test, "e" );
pragma assert( n = 2 );
n := strings.index( test, "e", direction.forward );
pragma assert( n = 2 );
n := strings.index( test, "s", direction.backward );
pragma assert( n = 3 );
n := strings.count( test, "t" );
pragma assert( n = 2 );
n := strings.index_non_blank( test, direction.backward );
pragma assert( n = 4 );
n := strings.index_non_blank( test );
pragma assert( n = 1 );
n := strings.index_non_blank( test, direction.forward ); -- n is 1 again
pragma assert( n = 1 );
s := strings.replace_slice( test, p, n, "b" );
pragma assert( s = "best" );
s := strings.insert( test, p, "s" );
pragma assert( s = "stest" );
s := strings.overwrite( test, p, "p" );
pragma assert( s = "pest" );
s := strings.delete( test, p, n );
pragma assert( s = "est" );
s := strings.trim( "  test  ", trim_end.both );
pragma assert( s = "test" );
s := strings.trim( "  test  ", trim_end.left );
pragma assert( s = "test  " );
s := strings.trim( "  test  ", trim_end.right );
pragma assert( s = "  test" );
s := strings.head( test, n );
pragma assert( s = "t" );
s := strings.head( test, 6, '*' );
pragma assert( s = "test**" );
s := strings.tail( test, n );
pragma assert( s = "t" );
s := strings.tail( test, 6, '*' );
pragma assert( s = "**test" );
n := strings.length( test );
pragma assert( n = 4 );
s := strings.image( 65 );
pragma assert( s = " 65" );
s := strings.image( true );
pragma assert( s = "true" );
c := strings.val( 65 );
pragma assert( c = 'A' );
s := strings.field( "a/b/c", 1, '/' );
pragma assert( s = "a" );
s := strings.field( "a/b/c", 3, '/' );
pragma assert( s = "c" );
s := strings.field( " ", 2, '/' );
pragma assert( s = "" );
s := strings.field( "a" & ASCII.CR & "b" & ASCII.CR & "c", 3 );
pragma assert( s = "c" );
s := strings.csv_field( "a/" & ASCII.Quotation & "b/c" & ASCII.Quotation &
  "/d", 1, '/' );
pragma assert( s = "a" );
s := strings.csv_field( "a/" & ASCII.Quotation & "b/c" & ASCII.Quotation &
  "/d", 2, '/' );
pragma assert( s = "b/c" );
s := strings.csv_field( "a/" & ASCII.Quotation & "b/c" & ASCII.Quotation &
  "/d", 3, '/' );
pragma assert( s = "d" );
s := strings.csv_field( "a/" & ASCII.Quotation & "b/c" & ASCII.Quotation &
  "/d", 4, '/' );
pragma assert( s = "" );
s := strings.csv_field( " ", 2, '/' );
pragma assert( s = "" );
s := strings.lookup( "a/b", "a", '/' );
pragma assert( s = "b" );
s := strings.lookup( "a/b", "c", '/' );
pragma assert( s = "" );
s := strings.lookup( "a/b/", "a", '/' );
pragma assert( s = "b" );
s := strings.lookup( "a/b/", "c", '/' );
pragma assert( s = "" );
s := strings.lookup( "a/b/c/d", "a", '/' );
pragma assert( s = "b" );
s := strings.lookup( "a/b/c/d", "b", '/' );
pragma assert( s = "" );
s := strings.lookup( "a/b/c/d", "c", '/' );
pragma assert( s = "d" );
s := "a/b/c";
strings.replace( s, 2, "j", '/' );
pragma assert( s = "a/j/c" );
strings.replace( s, 1, "i", '/' );
pragma assert( s = "i/j/c" );
strings.replace( s, 3, "k", '/' );
pragma assert( s = "i/j/k" );
s := "";
strings.replace( s, 3, "k", '/' );
pragma assert( s = "" );
s := "a" & ASCII.CR & "b" & ASCII.CR & "c";
strings.replace( s, 2, "j" );
pragma assert( s = "a" & ASCII.CR & "j" & ASCII.CR & "c" );
s := "a/b/c";
strings.csv_replace( s, 2, "j", '/' );
pragma assert( s = "a/j/c" );
strings.csv_replace( s, 1, "i", '/' );
pragma assert( s = "i/j/c" );
strings.csv_replace( s, 3, "k", '/' );
pragma assert( s = "i/j/k" );
s := "";
strings.csv_replace( s, 3, "k", '/' );
pragma assert( s = "" );
s := "a,b,c";
strings.csv_replace( s, 2, "j" );
pragma assert( s = "a,j,c" );
s := "a/" & ASCII.Quotation & "b" & ASCII.Quotation & "/c";
strings.csv_replace( s, 2, "j", '/' );
pragma assert( s = "a/j/c" );
s := "a/" & ASCII.Quotation & "b" & ASCII.Quotation & "/c";
strings.csv_replace( s, 3, "k", '/' );
pragma assert( s = "a/" & ASCII.Quotation & "b" & ASCII.Quotation & "/k" );
s := "a/b/c";
strings.csv_replace( s, 2, "j/k", '/' );
pragma assert( s = "a/" & ASCII.Quotation & "j/k" & ASCII.Quotation & "/c" );
s := "a/" & ASCII.Quotation & "b" & ASCII.Quotation & "/c";
strings.csv_replace( s, 2, "j/k", '/' );
pragma assert( s = "a/" & ASCII.Quotation & "j/k" & ASCII.Quotation & "/c" );
s := strings.to_lower( "TeST" );
pragma assert( s = "test" );
s := strings.to_lower( "" );
pragma assert( s = "" );
s := strings.to_upper( "tEst" );
pragma assert( s = "TEST" );
s := strings.to_upper( "" );
pragma assert( s = "" );
s := strings.to_proper( "tEst" );
pragma assert( s = "Test" );
s := strings.to_proper( "" );
pragma assert( s = "" );
s := strings.to_basic( "tEst" );
pragma assert( s = "tEst" );
s := strings.to_basic( "" );
pragma assert( s = "" );
s := strings.to_escaped( "A" & ASCII.CR );
pragma assert( s = "A[# 13]" );
s := strings.to_escaped( "" );
pragma assert( s = "" );
left_split, right_split : string;
s := "hello there";
strings.split( s, left_split, right_split, 0 );
pragma assert( left_split = "" );
pragma assert( right_split = s );
strings.split( s, left_split, right_split, 100 );
pragma assert( left_split = s );
pragma assert( right_split = "" );
strings.split( s, left_split, right_split, 9 );
pragma assert( left_split = "hello " );
pragma assert( right_split = "there" );
s := "hello_there";
strings.split( s, left_split, right_split, 9 );
pragma assert( left_split = "hello_the" );
pragma assert( right_split = "re" );
strings.split( s, left_split, right_split, 9 );
b := strings.is_control( ASCII.CR );
pragma assert( b = true );
b := strings.is_control( "A" );
pragma assert( b = false );
b := strings.is_control( ASCII.CR & ASCII.CR );
pragma assert( b = true );
b := strings.is_control( "A" & ASCII.CR );
pragma assert( b = false );
b := strings.is_graphic( ASCII.CR );
pragma assert( b = false );
b := strings.is_graphic( "A" );
pragma assert( b = true );
b := strings.is_graphic( ASCII.CR & ASCII.CR );
pragma assert( b = false );
b := strings.is_graphic( "A" & ASCII.CR );
pragma assert( b = false );
b := strings.is_letter( ASCII.CR );
pragma assert( b = false );
b := strings.is_letter( "A" );
pragma assert( b = true );
b := strings.is_letter( "Aa" );
pragma assert( b = true );
b := strings.is_letter( "A" & ASCII.CR );
pragma assert( b = false );
b := strings.is_lower( ASCII.CR );
pragma assert( b = false );
b := strings.is_lower( "a" );
pragma assert( b = true );
b := strings.is_lower( "Aa" );
pragma assert( b = false );
b := strings.is_lower( "A" );
pragma assert( b = false );
b := strings.is_upper( ASCII.CR );
pragma assert( b = false );
b := strings.is_upper( "A" );
pragma assert( b = true );
b := strings.is_upper( "Aa" );
pragma assert( b = false );
b := strings.is_upper( "AB" );
pragma assert( b = true );
b := strings.is_basic( ASCII.CR );
pragma assert( b = false );
b := strings.is_basic( "A" );
pragma assert( b = true );
b := strings.is_basic( "Aa" );
pragma assert( b = true );
b := strings.is_basic( "A" & ASCII.CR );
pragma assert( b = false );
b := strings.is_digit( ASCII.CR );
pragma assert( b = false );
b := strings.is_digit( "A" );
pragma assert( b = false );
b := strings.is_digit( "1" );
pragma assert( b = true );
b := strings.is_digit( "12" );
pragma assert( b = true );
b := strings.is_hexadecimal_digit( ASCII.CR );
pragma assert( b = false );
b := strings.is_hexadecimal_digit( "G" );
pragma assert( b = false );
b := strings.is_hexadecimal_digit( "1" );
pragma assert( b = true );
b := strings.is_hexadecimal_digit( "1A" );
pragma assert( b = true );
b := strings.is_alphanumeric( ASCII.CR );
pragma assert( b = false );
b := strings.is_alphanumeric( "G" );
pragma assert( b = true );
b := strings.is_alphanumeric( "1" );
pragma assert( b = true );
b := strings.is_alphanumeric( "1-" );
pragma assert( b = false );
b := strings.is_special( ASCII.CR );
pragma assert( b = false );
b := strings.is_special( "G" );
pragma assert( b = false );
b := strings.is_special( "1" );
pragma assert( b = false );
b := strings.is_special( "1-" );
pragma assert( b = false );
b := strings.is_slashed_date( "" );
pragma assert( b = false );
b := strings.is_slashed_date( "1976" );
pragma assert( b = false );
b := strings.is_slashed_date( "xyzzy" );
pragma assert( b = false );
b := strings.is_slashed_date( "xx/xx/xx" );
pragma assert( b = false );
b := strings.is_slashed_date( "xx/xx/xxxx" );
pragma assert( b = false );
b := strings.is_slashed_date( "00x00/00" );
pragma assert( b = false );
b := strings.is_slashed_date( "00/00x00" );
pragma assert( b = false );
b := strings.is_slashed_date( "00x00/0000" );
pragma assert( b = false );
b := strings.is_slashed_date( "00/00x0000" );
pragma assert( b = false );
b := strings.is_slashed_date( "00/0x/00" );
pragma assert( b = false );
b := strings.is_slashed_date( "00/0x/0000" );
pragma assert( b = false );
b := strings.is_slashed_date( "00/00/00" );
pragma assert( b = true );
b := strings.is_slashed_date( "000/00/00" );
pragma assert( b = false );
b := strings.is_slashed_date( "00/000/00" );
pragma assert( b = false );
b := strings.is_slashed_date( "000/00/0000" );
pragma assert( b = false );
b := strings.is_slashed_date( "00/000/0000" );
pragma assert( b = false );
b := strings.is_slashed_date( "00/00/000" );
pragma assert( b = false );
b := strings.is_slashed_date( "00/00/0000" );
pragma assert( b = true );
b := strings.is_slashed_date( "99/99/99" );
pragma assert( b = true );
b := strings.is_slashed_date( "99/99/9999" );
pragma assert( b = true );
b := strings.is_fixed( "" );
pragma assert( b = false );
b := strings.is_fixed( "x" );
pragma assert( b = false );
b := strings.is_fixed( "x.x" );
pragma assert( b = false );
b := strings.is_fixed( ".0" );
pragma assert( b = false );
b := strings.is_fixed( "0." );
pragma assert( b = false );
b := strings.is_fixed( "0..0" );
pragma assert( b = false );
b := strings.is_fixed( "0.0" );
pragma assert( b = true );
b := strings.is_fixed( "999.999" );
pragma assert( b = true );
unbs := strings.to_unbounded_string( "bar" );
pragma assert( unbs = "bar" );
s := strings.to_string( unbs );
pragma assert( s = "bar" );
b := strings.is_typo_of( "foob", "fozb" );
pragma assert( b = true );
b := strings.is_typo_of( "foobs", "fobs" );
pragma assert( b = true );
b := strings.is_typo_of( "foob", "ffoob" );
pragma assert( b = true );
b := strings.is_typo_of( "ofob", "foob" );
pragma assert( b = true );
strings.set_unbounded_string( unbs, "foo" );
pragma assert( unbs = "foo" );
unbs := strings.unbounded_slice( "foobar", 2, 4 );
pragma assert( unbs = "oob" );
s := strings.mktemp( "test" );
pragma assert( s /= "./test" );

sb64 : strings.base64_string;
sb64 := strings.to_base64( "foobar" );
pragma assert( sb64 = "Zm9vYmFy" );
s := strings.to_string( sb64 );
pragma assert( s = "foobar" );

-- files package

b := files.exists( "goodtest.sp" );
pragma assert( b = true );
b := files.exists( "xyzzy.foobar" );
pragma assert( b = false );
b := files.is_absolute_path( "/tmp/" );
pragma assert( b = true );
b := files.is_absolute_path( "." );
pragma assert( b = false );
b := files.is_absolute_path( "" );
pragma assert( b = false );
b := files.is_regular_file( "goodtest.sp" );
pragma assert( b = true );
b := files.is_regular_file( "cdtest" );
pragma assert( b = false );
b := files.is_regular_file( "xyzzy.foobar" );
pragma assert( b = false );
b := files.is_directory( "goodtest.sp" );
pragma assert( b = false );
b := files.is_directory( "cdtest" );
pragma assert( b = true );
b := files.is_directory( "xyzzy.foobar" );
pragma assert( b = false );
b := files.is_directory( "" );
pragma assert( b = false );
b := files.is_readable_file( "goodtest.sp" );
pragma assert( b = true );
b := files.is_readable_file( "write_only.txt" );
pragma assert( b = false );
b := files.is_readable_file( "exec_only.txt" );
pragma assert( b = false );
b := files.is_readable_file( "cdtest" );
pragma assert( b = false );
b := files.is_readable_file( "xyzzy.foobar" );
pragma assert( b = false );
b := files.is_readable_file( "" );
pragma assert( b = false );
b := files.is_readable( "goodtest.sp" );
pragma assert( b = true );
b := files.is_readable( "write_only.txt" );
pragma assert( b = false );
b := files.is_readable( "exec_only.txt" );
pragma assert( b = false );
b := files.is_readable( "cdtest" );
pragma assert( b = true );
b := files.is_readable( "xyzzy.foobar" );
pragma assert( b = false );
b := files.is_readable( "" );
pragma assert( b = false );
b := files.is_writable_file( "goodtest.sp" );
pragma assert( b = false );
b := files.is_writable_file( "write_only.txt" );
pragma assert( b = true );
b := files.is_writable_file( "exec_only.txt" );
pragma assert( b = false );
b := files.is_writable_file( "cdtest" );
pragma assert( b = false );
b := files.is_writable_file( "xyzzy.foobar" );
pragma assert( b = false );
b := files.is_writable_file( "" );
pragma assert( b = false );
b := files.is_writable( "goodtest.sp" );
pragma assert( b = false );
b := files.is_writable( "write_only.txt" );
pragma assert( b = true );
b := files.is_writable( "exec_only.txt" );
pragma assert( b = false );
b := files.is_writable( "cdtest" );
pragma assert( b = true );
b := files.is_writable( "xyzzy.foobar" );
pragma assert( b = false );
b := files.is_writable( "" );
pragma assert( b = false );
b := files.is_executable_file( "goodtest.sp" );
pragma assert( b = false );
b := files.is_executable_file( "cdtest" );
pragma assert( b = false );
b := files.is_executable_file( "write_only.txt" );
pragma assert( b = false );
b := files.is_executable_file( "exec_only.txt" );
pragma assert( b = true );
b := files.is_executable_file( "xyzzy.foobar" );
pragma assert( b = false );
b := files.is_executable_file( "" );
pragma assert( b = false );
b := files.is_executable( "goodtest.sp" );
pragma assert( b = false );
b := files.is_executable( "cdtest" );
pragma assert( b = true );
b := files.is_executable( "write_only.txt" );
pragma assert( b = false );
b := files.is_executable( "exec_only.txt" );
pragma assert( b = true );
b := files.is_executable( "xyzzy.foobar" );
pragma assert( b = false );
b := files.is_executable_file( "" );
pragma assert( b = false );
b := files.is_waiting_file( "goodtest.sp" );
pragma assert( b = true );
b := files.is_waiting_file( "write_only.txt" );
pragma assert( b = false );
s := files.basename( "dir/file" );
pragma assert( s = "file" );
s := files.basename( "file" );
pragma assert( s = "file" );
s := files.basename( "" );
pragma assert( s = "" );
s := files.dirname( "dir/file" );
pragma assert( s = "dir" );
s := files.dirname( "file" );
pragma assert( s = "." );
s := files.dirname( "" );
pragma assert( s = "." );
li := files.size( "write_only.txt" );
pragma assert( li = 0 );
li := files.size( "goodtest.sp" );
pragma assert( li > 0 );
-- last_modified/last_changed under calendar section

-- built-in shell functions

i := $?; -- status
i := $#; -- number of arguments
pragma assert( i = 3 );
i := $$; -- pid
i := $!; -- last pid
s := $0; -- command name
s := $1; -- first argument
pragma assert( s = "a" );

n := command_line.argument_count;
pragma assert( n = 3 );
s := command_line.command_name;
s := command_line.argument( 1 );
pragma assert( s = "a" );
n := command_line.environment.environment_count;
pragma assert( n > 0 ); -- can't know exactly how many
s := command_line.environment.environment_value( 1 );
pragma assert( strings.length( s ) > 0 ); -- can't know exactly what

os.system( "cd ." );
i := os.status;
pragma assert( i = 0 );
n := os.pid;
pragma assert( n /= 0 );
n := $!;
pragma assert( n = os.last_child );
s := os.error_string( 1 );
pragma assert( strings.length( s ) > 0 );

-- cd command
-- note path is only shown in interactive modes

s1 := PWD;
s := PWD & "/" & "cdtest";
cd .;
pragma assert( PWD = OLDPWD );
cd cdtest;
pragma assert( PWD = s );
pragma assert( OLDPWD = s1 );
cd -;
pragma assert( PWD = s1 );
pragma assert( OLDPWD = s );
cd "";
pragma assert( PWD = HOME );
pragma assert( OLDPWD = s1 );
cd -;
cd ~;
pragma assert( PWD = HOME );
pragma assert( OLDPWD = s1 );
cd -;
-- AdaScript style
cd( "." );
pragma assert( PWD = OLDPWD );
cd( "cdtest" );
pragma assert( PWD = s );
pragma assert( OLDPWD = s1 );
cd( "-" );
pragma assert( PWD = s1 );
pragma assert( OLDPWD = s );
cd( "" );
pragma assert( PWD = HOME );
pragma assert( OLDPWD = s1 );
cd( "-" );
cd( "~" );
pragma assert( PWD = HOME );
pragma assert( OLDPWD = s1 );
cd( "-" );

-- basic text_io and % (last output)

put( "this " );
pragma assert( % = "this " );
put( standard_output, "is " );
pragma assert( % = "is " );
put_line( "a test" );
pragma assert( % = "a test" );
put_line( standard_error, "standard error" );
new_line;
put( 123.45, "####9.99" );
pragma assert( % = " $123.45" );
? "a test";
pragma assert( % = "a test" );
? "another test";
pragma assert( % = "another test" );
? "hello" @ "world";
pragma assert( % = "world" );

-- null statement

null;

-- delay statement

delay 0.5;

-- more basic statements

history;
history 1;
history 999;
history -c;

-- basic pragmas (ada_95 tested below)

pragma annotate( "This is a test" );
pragma depreciated( "<This is not an error>" );
type import_string is new string;
FOOBAR : limited import_string;
pragma import( shell, FOOBAR );
pragma export( shell, FOOBAR );
pragma volatile( FOOBAR );
foo_dummy_variable_123: string := "foo_123";
pragma unchecked_import( shell, foo_dummy_variable_123 );
pragma assert( foo_dummy_variable_123 = "foo_123" );
FOOBAR1 : limited import_string;
FOOBAR2 : import_string;
pragma import( shell, FOOBAR1 ) @ ( shell, FOOBAR2 );
pragma assert( FOOBAR2 = "foobar" );
FOOBAR3 : limited import_string;
FOOBAR4 : import_string;

team : limited teams.member;
ken  : teams.member;

pragma advise( ken, team, "i need some advice" );
pragma blocked( ken, team, "stuck on some task" );
pragma clarify( ken, team, "explain some issue" );
pragma dispute( team, ken, "question some design" );
pragma propose( team, ken, "suggest some solution" );
pragma refactor( team, ken, "cleanup some code" );

pragma suppress( all_todos_for_release );
pragma todo( ken, "some task 1", work_measure.unknown, 0, work_priority.level, 'l', "ID 1" );
pragma todo( ken, "some task 2", work_measure.hours, 2, work_priority.level, 'l', "ID 1" );
pragma todo( ken, "some task 3", work_measure.function_points, 3, work_priority.level, 'l', "ID 1" );
pragma todo( ken, "some task 4", work_measure.story_points, 5, work_priority.level, 'l', "ID 2" );
pragma todo( ken, "some task 5", work_measure.lines_of_code, 8, work_priority.level, 'l', "ID 3" );
pragma todo( ken, "some task 6", work_measure.size, "s", work_priority.level, 'l', "ID 4" );
pragma todo( ken, "some task 7", work_measure.size, "m", work_priority.level, 'l', "ID 5" );
pragma todo( ken, "some task 8", work_measure.size, "l", work_priority.level, 'l', "ID 6" );
pragma todo( ken, "some task 9", work_measure.size, "xl", work_priority.level, 'l', "ID 7" );

pragma todo( ken, "some task 10", work_measure.unknown, 0, work_priority.unknown, 0, "ID 8" );
pragma todo( ken, "some task 11", work_measure.unknown, 0, work_priority.level, 'm', "ID 9" );
pragma todo( ken, "some task 12", work_measure.unknown, 0, work_priority.level, 'h', "ID 10" );
pragma todo( ken, "some task 13", work_measure.unknown, 0, work_priority.severity, 1, "ID 11" );
pragma todo( ken, "some task 14", work_measure.unknown, 0, work_priority.severity, 2, "ID 12" );
pragma todo( ken, "some task 15", work_measure.unknown, 0, work_priority.severity, 3, "ID 13" );
pragma todo( ken, "some task 16", work_measure.unknown, 0, work_priority.severity, 4, "ID 14" );
pragma todo( ken, "some task 17", work_measure.unknown, 0, work_priority.severity, 5, "ID 15" );
pragma todo( ken, "some task 18", work_measure.unknown, 0, work_priority.risk, 35, "ID 16" );
pragma todo( ken, "some task 19", work_measure.unknown, 0, work_priority.cvss, 5.5, "ID 17" );

pragma todo( ken, "some task 18", work_measure.unknown, 0, work_priority.unknown, 0 );

-- static epxressions

pragma annotate( "foo" & "bar" );
pragma annotate( 'f' );
pragma annotate( System.System_Version );
pragma annotate( author, "foo" & "bar" );
pragma advise( ken, ken, "foo" & "bar" );
pragma blocked( ken, ken, "foo" & "bar" );
pragma clarify( ken, ken, "foo" & "bar" );
pragma depreciated( "foo" & "bar" );
pragma deprecated( "foo" & "bar" );
pragma dispute( ken, ken, "foo" & "bar" );
pragma propose( ken, ken, "foo" & "bar" );
pragma refactor( ken, ken, "foo" & "bar" );
pragma todo( ken, "some " & "task 1", work_measure.unknown, 0, work_priority.level, 'l', "ID 1" );

-- right now all static expressions are strings.  This is a workaround.
-- also, no way to test if result is correct.  This, though, uses a
-- non-static expression in strings.image...

--pragma annotate( description, strings.image( 4 ) );
--pragma annotate( description, strings.image( 0 ) );
--pragma annotate( description, strings.image( 1000000 ) );
--pragma annotate( description, strings.image( 1_000_000 ) );
--pragma annotate( description, strings.image( 0.0 ) );
--pragma annotate( description, strings.image( +1 ) );
--pragma annotate( description, strings.image( -1 ) );
--pragma annotate( description, strings.image( 1 + 1 ) );
--pragma annotate( description, strings.image( 1 * 1 ) );
--pragma annotate( description, strings.image( 1 ** 1 ) );
--pragma annotate( description, strings.image( 1 / 1 ) );
--pragma annotate( description, strings.image( 1 - 1 ) );
--pragma annotate( description, strings.image( (1 + 1 ) / 1 ) );
--pragma annotate( description, strings.image( ((1 + 1 )) / 1 ) );
--pragma annotate( description, strings.image( (1+1)/2 ) );
--pragma annotate( description, strings.image( 3 mod 2 ) );
--pragma annotate( description, strings.image( 3 rem 2 ) );
--pragma annotate( description, 3 * "str" );
--pragma annotate( description, strings.image( true ) );
--pragma annotate( description, strings.image( false ) );
--pragma annotate( description, strings.image( not true ) );
--pragma annotate( description, strings.image( not false ) );
--pragma annotate( description, strings.image( true and true ) );
--pragma annotate( description, strings.image( true and false ) );
--pragma annotate( description, strings.image( false and true ) );
--pragma annotate( description, strings.image( false and false ) );
--pragma annotate( description, strings.image( true and false and true ) );
--pragma annotate( description, strings.image( false and true and false ) );
--pragma annotate( description, strings.image( true and true and true ) );
--pragma annotate( description, strings.image( false and false and false ) );
--pragma annotate( description, strings.image( true or true ) );
--pragma annotate( description, strings.image( true or false ) );
--pragma annotate( description, strings.image( false or true ) );
--pragma annotate( description, strings.image( false or false ) );
--pragma annotate( description, strings.image( true or false or true ) );
--pragma annotate( description, strings.image( false or true or false ) );
--pragma annotate( description, strings.image( true or true or true ) );
--pragma annotate( description, strings.image( false or false or false ) );
--pragma annotate( description, strings.image( true xor true ) );
--pragma annotate( description, strings.image( true xor false ) );
--pragma annotate( description, strings.image( false xor true ) );
--pragma annotate( description, strings.image( false xor false ) );
--pragma annotate( description, strings.image( 1 > 1 ) );
--pragma annotate( description, strings.image( 1 = 1 ) );
--pragma annotate( description, strings.image( 1 < 1 ) );
--pragma annotate( description, strings.image( 1 >= 1 ) );
--pragma annotate( description, strings.image( 1 <= 1 ) );
--pragma annotate( description, strings.image( 1 = 1 ) );
--pragma annotate( description, strings.image( 1 /= 1 ) );
--pragma annotate( description, strings.image( "1" > "1" ) );
--pragma annotate( description, strings.image( "1" = "1" ) );
--pragma annotate( description, strings.image( "1" < "1" ) );
--pragma annotate( description, strings.image( "1" >= "1" ) );
--pragma annotate( description, strings.image( "1" <= "1" ) );
--pragma annotate( description, strings.image( "1" = "1" ) );
--pragma annotate( description, strings.image( "1" /= "1" ) );

-- team member fields

ken.description := "my description";
ken.skills := "my skills";
ken.lang := "my lang";
ken.id := 1;
ken.preferred_contact := "my pc";
ken.email := "k@b";
ken.secondary_email := "k@b2";
ken.preferred_name := "gonzo";
ken.full_name := "Ken Burtch";
ken.chair := "3rd from door";
ken.nickname := "master of magic";
ken.business_phone := "(555) 555-5555";
ken.messenging := "skype: foobar";
ken.teams := "architecture";
ken.roles := "senior architect";
ken.active := true;

-- pragma block

pragma is
  annotate( description, "me" );
  import( shell, FOOBAR3 ) @ ( shell, FOOBAR4 );
end pragma;
pragma assert( FOOBAR4 = "foobar" );

-- json pragmas

type import_integer is new integer;
FOOBAR_INT : import_integer;
pragma import_json( shell, FOOBAR_INT );
pragma assert( FOOBAR_INT = 98 );
FOOBAR_STRING : import_string;
pragma import_json( shell, FOOBAR_STRING );
pragma export_json( shell, FOOBAR_STRING );
pragma assert( FOOBAR_STRING = "foobar" );
pragma volatile( FOOBAR_STRING );
type FIA_TYPE is array(1..1) of integer;
FOOBAR_INT_ARRAY : FIA_TYPE;
pragma import_json( shell, FOOBAR_INT_ARRAY );
pragma assert( FOOBAR_INT_ARRAY(1) = 32 );
type FR_TYPE is record
  s : string;
end record;
FOOBAR_RECORD : FR_TYPE;
pragma import_json( shell, FOOBAR_RECORD );
pragma assert( FOOBAR_RECORD.s = "foo" );

FOOBAR_INT_UNCHECKED : integer;
pragma unchecked_import_json( shell, FOOBAR_INT_UNCHECKED );
pragma assert( FOOBAR_INT_UNCHECKED = 98 );
FOOBAR_STRING_UNCHECKED : string;
pragma unchecked_import_json( shell, FOOBAR_STRING_UNCHECKED );
pragma assert( FOOBAR_STRING_UNCHECKED = "foobar" );
type FIAU_TYPE is array(1..1) of integer;
FOOBAR_INT_ARRAY_UNCHECKED : FIAU_TYPE;
pragma unchecked_import_json( shell, FOOBAR_INT_ARRAY_UNCHECKED );
pragma assert( FOOBAR_INT_ARRAY_UNCHECKED(1) = 32 );
type FRU_TYPE is record
  s : string;
end record;
FOOBAR_RECORD_UNCHECKED : FRU_TYPE;
pragma unchecked_import_json( shell, FOOBAR_RECORD_UNCHECKED );
pragma assert( FOOBAR_RECORD_UNCHECKED.s = "foo" );
-- unchecked imports on non-existent variables, no error and leave old values
FOOBAR_INT_UNCHECKED_NOEXIST : integer := 97;
pragma unchecked_import_json( shell, FOOBAR_INT_UNCHECKED_NOEXIST );
pragma assert( FOOBAR_INT_UNCHECKED_NOEXIST = 97 );
FOOBAR_STRING_UNCHECKED_NOEXIST : string := "undefined";
pragma unchecked_import_json( shell, FOOBAR_STRING_UNCHECKED_NOEXIST );
pragma assert( FOOBAR_STRING_UNCHECKED_NOEXIST = "undefined" );
FOOBAR_INT_ARRAY_UNCHECKED_NOEXIST : FIAU_TYPE;
FOOBAR_INT_ARRAY_UNCHECKED_NOEXIST(1) := 21;
pragma unchecked_import_json( shell, FOOBAR_INT_ARRAY_UNCHECKED_NOEXIST );
pragma assert( FOOBAR_INT_ARRAY_UNCHECKED_NOEXIST(1) = 21 );
FOOBAR_RECORD_UNCHECKED_NOEXIST : FRU_TYPE;
FOOBAR_RECORD_UNCHECKED_NOEXIST.s := "undefined again";
pragma unchecked_import_json( shell, FOOBAR_RECORD_UNCHECKED_NOEXIST );
pragma assert( FOOBAR_RECORD_UNCHECKED_NOEXIST.s = "undefined again" );

-- enums package

pragma assert( enums.first( boolean ) = false );
pragma assert( enums.last( boolean ) = true );
pragma assert( enums.pred( true ) = false );
pragma assert( enums.succ( false ) = true );

pragma assert( enums.first( new_enum2 ) = enum1 );
pragma assert( enums.last( new_enum2 ) = enum3 );
pragma assert( enums.pred( enum2 ) = enum1 );
pragma assert( enums.succ( enum2 ) = enum3 );

pragma assert( enums.first( new_enum ) = enum0 );
pragma assert( enums.last( new_enum ) = enum0 );

-- arrays

anonarray : array(1..10) of integer;
anonarray(1) := 1;
pragma assert( anonarray(1) = 1 );
anonarray(10) := 2;
pragma assert( anonarray(10) = 2 );

anonarray2 : array(-10..-1) of integer;
anonarray2(-1) := 1;
pragma assert( anonarray2(-1) = 1 );
anonarray2(-10) := 2;
pragma assert( anonarray2(-10) = 2 );

type arrtype is array(1900..1999) of integer;
typedarray : arrtype;
typedarray(1900) := 3;
pragma assert( typedarray(1900) = 3 );
typedarray(1999) := 4;
pragma assert( typedarray(1999) = 4 );

type shortarray is array(1..1) of integer;
sa1 : shortarray := (5);
pragma assert( sa1(1) = 5 );
sa2 : shortarray := sa1;
pragma assert( sa2(1) = 5 );

-- null array

type nullarray is array(1..0) of integer;
nularr : nullarray;

type constantarray is constant array(1..2) of integer;

--constarrayvar : constant nullarray; -- a little pointless...
--pragma assumption( used, constarrayvar );

type limitedarray is limited array(1..2) of integer;

limitedarrayvar : limited constantarray;
pragma assumption( used, limitedarrayvar );

limitedarrayvar2 : limitedarray;
pragma assumption( used, limitedarrayvar2 );

type abstractarray is abstract array(1..2) of integer;


-- type of an array type

type arrtype2 is new arrtype;
typed2a : limited arrtype2;
pragma assert( arrays.first( typed2a ) = 1900 );
pragma assert( arrays.last( typed2a ) = 1999 );
pragma assert( arrays.length( typed2a ) = 100 );

-- subtype of an array type

subtype subbedarray is arrtype;
suba1 : limited subbedarray;
pragma assert( arrays.first( suba1 ) = 1900 );
pragma assert( arrays.last( suba1 ) = 1999 );
pragma assert( arrays.length( suba1 ) = 100 );

-- array attributes

pragma assert( arrays.first( arrtype ) = 1900 );
pragma assert( arrays.first( typedarray ) = 1900 );
pragma assert( arrays.last( arrtype ) = 1999 );
pragma assert( arrays.last( typedarray ) = 1999 );
pragma assert( arrays.length( arrtype ) = 100 );
pragma assert( arrays.length( typedarray ) = 100 );
pragma assert( arrays.first( nullarray ) = 1 );
pragma assert( arrays.first( nularr ) = 1 );
pragma assert( arrays.last( nullarray ) = 0 );
pragma assert( arrays.last( nularr ) = 0 );
pragma assert( arrays.length( nullarray ) = 0 );
pragma assert( arrays.length( nularr ) = 0 );

-- enum arrays

type arrayenum is (aenum1, aenum2, aenum3 );
type longerarray is array(aenum1..aenum3) of integer;
la1 : longerarray := (1,2,3);
pragma assert( la1(aenum1) = 1 );
pragma assert( la1(aenum2) = 2 );
pragma assert( la1(aenum3) = 3 );

subtype longerarray2 is longerarray;
la2 : limited longerarray2 := la1;

lae : arrayenum;

lae := arrays.first( la1 );
pragma assert( lae = aenum1 );
lae := arrays.last( la1 );
pragma assert( lae = aenum3 );
n := arrays.length( la1 );
pragma assert( n = 3 );
j := arrays.first( sa1 );
pragma assert( j = 1 );
k := arrays.last( sa1 );
pragma assert( k = 1 );
n := arrays.length( sa1 );
pragma assert( n = 1 );
lae := arrays.first( la2 );
pragma assert( lae = aenum1 );
lae := arrays.last( la2 );
pragma assert( lae = aenum3 );
n := arrays.length( la2 );
pragma assert( n = 3 );
arrays.flip( la1 );
pragma assert( la1(aenum1) = 3 );
pragma assert( la1(aenum2) = 2 );
pragma assert( la1(aenum3) = 1 );
arrays.rotate_left( la1 );
pragma assert( la1(aenum1) = 2 );
pragma assert( la1(aenum2) = 1 );
pragma assert( la1(aenum3) = 3 );
arrays.rotate_right( la1 );
pragma assert( la1(aenum1) = 3 );
pragma assert( la1(aenum2) = 2 );
pragma assert( la1(aenum3) = 1 );
arrays.shift_left( la1 );
pragma assert( la1(aenum1) = 2 );
pragma assert( la1(aenum2) = 1 );
pragma assert( la1(aenum3) = 1 );
arrays.shift_right( la1 );
pragma assert( la1(aenum1) = 2 );
pragma assert( la1(aenum2) = 2 );
pragma assert( la1(aenum3) = 1 );
arrays.shuffle( la1 );
-- can't do much to test shuffle
-- make sure null array doesn't crash
arrays.flip( nularr );
arrays.rotate_left( nularr );
arrays.rotate_right( nularr );
arrays.shift_left( nularr );
arrays.shift_right( nularr );
arrays.shuffle( nularr );
la1(aenum1) := 1;
la1(aenum2) := 2;
la1(aenum3) := 3;
arrays.bubble_sort( la1 );
pragma assert( la1(aenum1) = 1 );
pragma assert( la1(aenum2) = 2 );
pragma assert( la1(aenum3) = 3 );
arrays.bubble_sort_descending( la1 );
pragma assert( la1(aenum1) = 3 );
pragma assert( la1(aenum2) = 2 );
pragma assert( la1(aenum3) = 1 );
arrays.heap_sort( la1 );
pragma assert( la1(aenum1) = 1 );
pragma assert( la1(aenum2) = 2 );
pragma assert( la1(aenum3) = 3 );
arrays.heap_sort_descending( la1 );
pragma assert( la1(aenum1) = 3 );
pragma assert( la1(aenum2) = 2 );
pragma assert( la1(aenum3) = 1 );

-- test on null arrays

arrays.bubble_sort( nularr );
arrays.bubble_sort_descending( nularr );
arrays.heap_sort( nularr );
arrays.heap_sort_descending( nularr );
arrays.shuffle( nularr );
arrays.rotate_left( nularr);
arrays.rotate_right( nularr );
arrays.shift_left( nularr);
arrays.shift_right( nularr );
arrays.flip( nularr );

-- stats

la1(aenum1) := 1;
la1(aenum2) := 2;
la1(aenum3) := 3;
i := stats.average( la1 );
pragma assert( i = 2 );
i := stats.sum( la1 );
pragma assert( i = 6 );
i := stats.min( la1 );
pragma assert( i = 1 );
i := stats.max( la1 );
pragma assert( i = 3 );

-- record

type arec0 is abstract record
     i : integer;
end record;

type arec1 is record
     i : integer;
end record;

type arec2 is record
     -- i,j : integer;
     i   : integer;
     j   : integer;
     k   : character;
end record;

type arec3 is constant record
     i : integer := 1;
end record;

type arec4 is limited record
     i : integer := 1;
end record;

rec1 : arec1;
rec2 : arec2;
rec3 : arec1 := (5);
rec4 : arec2 := (6,7,'8');
rec5 : arec2 := rec4;
rec6, rec7 : arec1 := (1);
rec8 : arec4 := (9);
pragma assumption( used, rec8 );

-- This will be an error: specification not complete
--rec8 : constant arec1;
--pragma assumption( used, rec8 );
rec9 : limited arec3;
pragma assumption( used, rec9 );

rec1.i := 1;
rec2.i := 2;
rec2.j := 3;
rec2.k := '4';
pragma assert( rec1.i = 1 );
pragma assert( rec2.i = 2 );
pragma assert( rec2.j = 3 );
pragma assert( rec2.k = '4' );
pragma assert( rec3.i = 5 );
pragma assert( rec4.i = 6 );
pragma assert( rec4.j = 7 );
pragma assert( rec4.k = '8' );
pragma assert( rec5.i = 6 );
pragma assert( rec5.j = 7 );
pragma assert( rec5.k = '8' );
pragma assert( rec6.i = 1 );
pragma assert( rec7.i = 1 );
rec6.i := 5;
rec7.i := 6;
pragma assert( rec6.i = 5 );
pragma assert( rec7.i = 6 );

-- to prevent warnings for unused identifiers
-- normally a record would be used somewhere but here we were only
-- assigning to components.

rec1 := rec3;
rec2 := rec4;
rec5 := rec4;
rec6 := rec7;

-- Permissible with SparForte

type odd_rec is abstract record
  i : limited integer;
  c : constant integer := 1;
end record;

-- complex

complex1 : complex;
complex1.re := 1.0;
complex1.im := 2.0;
pragma assert( complex1.re = 1.0 );
pragma assert( complex1.im = 2.0 );
complex2 : complex := (1.0,2.0);
pragma assert( complex2.re = 1.0 );
pragma assert( complex2.im = 2.0 );
numerics.set_re( complex2, 3.0 );
numerics.set_im( complex2, 4.0 );
pragma assert( complex2.re = 3.0 );
pragma assert( complex2.im = 4.0 );
real_val : long_float;
real_val := numerics.modulus( complex1 );
real_val := numerics.argument( complex1 );
real_val := numerics.argument( complex1,1 );

-- declare block w/out declare

begin
  null;
end;

-- declare block with nested declarations

i := 0;
declare
  type int1 is new abstract integer;
  subtype int2 is int1;
  i : int2 := 1;
  path2 : string := PATH; -- global path
  PATH  : import_string; -- overshadows global path
  pragma import( shell, PATH );
  pragma assert( string( PATH ) = path2 );
begin
  declare
    i : integer := 2;
  begin
     pragma assert( i=2 );
  end;
  pragma assert( i=1 );
end;
pragma assert( i=0 );

-- user-defined procedure tests

i := 0;
declare
   procedure proc1 is
   begin
     null;
   end proc1;

   procedure proc2 is
   begin
     i := i + 1;
   end proc2;
   pragma assert( i = 0 ); -- no execution of proc2
begin
   proc1;
   pragma assert( i = 0 ); -- execution of proc2

   proc2;
   pragma assert( i = 1 ); -- execution of proc2
end;

declare
  -- TEMPORARILY DISABLED
  -- procedure proc3;
  procedure proc3 is
  begin
    i := i - 1;
  end proc3;
  pragma assert( i = 1 ); -- no execution of proc3
begin
  proc3;
  pragma assert( i = 0 ); -- execution of proc2
end;

declare
  procedure proc4 is
    procedure proc5 is
    begin
      i := i + 3;
    end proc5;
  begin
    proc5;
    i := i + 5;
  end proc4;
  pragma assert( i = 0 ); -- no execution of procs
begin
  proc4;                  -- call procedures
  pragma assert( i = 8 ); -- execution of proc4 and proc5
end;

declare
  procedure proc6 is
  begin
    begin
       i := i + 1;
    end;
  end proc6;
  pragma assert( i = 8 );
begin
  proc6;
  pragma assert( i = 9 ); -- execution of proc6 and nested begin block
end;

-- self-referential variables

i := 1;
declare
   i : integer := i+2;
   pragma assert( i=3 );
begin
   null;
end;

-- json

declare
  i0 : array( 1..0 ) of integer;
  i1 : array( 1..1 ) of integer := (23);
  i2 : array( 2..2 ) of integer := (24);
  i3 : array( 1..3 ) of integer := (25,26,27);
  s0 : array( 1..0 ) of string;
  s1 : array( 1..1 ) of string  := ("arch");
  s2 : array( 2..2 ) of string  := ("able");
  s3 : array( 1..3 ) of string  := ("adam","atom","axe");

  type json_test_enum is ( jte1, jte2, jte3 );

  subtype alt_boolean is boolean;

  --js : json_string; is global

  type json_arr1 is array(1..1) of string;
  ja : json_arr1;

  type json_arr2 is array(1..1) of integer;
  ja2 : json_arr2;

  type json_arr3 is array(1..1) of boolean;
  ja3 : json_arr3;

  type json_arr4 is array(1..1) of alt_boolean;
  ja4 : json_arr4;

  type json_arr5 is array(1..1) of json_test_enum;
  ja5 : json_arr5;

  type json_arr6 is array(1..2) of json_string;
  ja6 : json_arr6;

  type json_arr7 is array(1..2) of json_string;
  ja7 : json_arr7;

  type json_arr8 is array(1..2) of json_string;
  ja8 : json_arr8;

  type json_rec1 is record
    s : string;
  end record;
  rs : json_rec1;

  type json_rec2 is record
    s : string;
    n : natural;
  end record;
  rs2 : json_rec2;

  type json_rec3 is record
    isTrue : boolean;
    isFalse : boolean;
  end record;
  rs3 : json_rec3;

  type json_rec4 is record
    enum : json_test_enum;
  end record;
  rs4 : json_rec4;

  type json_rec5 is record
    isTrue : alt_boolean;
    isFalse : alt_boolean;
  end record;
  rs5 : json_rec5;

  type json_rec6 is record
    i : integer;
    a : json_string;
  end record;
  rs6 : json_rec6;

  type json_rec7 is record
    i : integer;
    s : string;
    j : json_string;
    k : json_string;
  end record;
  rs7 : json_rec7;

  type json_rec8 is record
    j : json_string;
  end record;
  rs8 : json_rec8;

begin
  -- to json
  arrays.to_json( js, i0 );
  pragma assert( js = "[]" );
  arrays.to_json( js, i1 );
  pragma assert( js = "[23]" );
  arrays.to_json( js, i2 );
  pragma assert( js = "[24]" );
  arrays.to_json( js, i3 );
  pragma assert( js = "[25,26,27]" );
  arrays.to_json( js, s0 );
  pragma assert( js = "[]" );
  arrays.to_json( js, s1 );
  pragma assert( js = "[" & ASCII.Quotation & "arch" & ASCII.Quotation & "]" );
  arrays.to_json( js, s2 );
  pragma assert( js = "[" & ASCII.Quotation & "able" & ASCII.Quotation & "]" );
  arrays.to_json( js, s3 );
  pragma assert( js = "[" &
     ASCII.Quotation & "adam" & ASCII.Quotation & "," &
     ASCII.Quotation & "atom" & ASCII.Quotation & "," &
     ASCII.Quotation & "axe" & ASCII.Quotation & "]" );
  -- from json
  arrays.to_array( i0, "[]" );
  arrays.to_array( i1, "[33]" );
  pragma assert( i1(1) = 33 );
  arrays.to_array( i2, "[34]" );
  pragma assert( i2(2) = 34 );
  arrays.to_array( i3, "[35,36,37]" );
  pragma assert( i3(1) = 35 );
  pragma assert( i3(2) = 36 );
  pragma assert( i3(3) = 37 );
  arrays.to_array( s0, "[]" );
  arrays.to_array( s1, "[" & ASCII.Quotation & "bear" & ASCII.Quotation & "]" );
  pragma assert( s1(1) = "bear" );
  arrays.to_array( s2, "[" & ASCII.Quotation & "bare" & ASCII.Quotation & "]" );
  pragma assert( s2(2) = "bare" );
  arrays.to_array( s3, "[" &
     ASCII.Quotation & "bug" & ASCII.Quotation & "," &
     ASCII.Quotation & "bat" & ASCII.Quotation & "," &
     ASCII.Quotation & "box" & ASCII.Quotation & "]" );
  pragma assert( s3(1) = "bug" );
  pragma assert( s3(2) = "bat" );
  pragma assert( s3(3) = "box" );
  -- special chars
  s1(1) := "" & ASCII.Quotation;
  arrays.to_json( js, s1 );
  pragma assert( js = "[" & ASCII.Quotation & "\" & ASCII.Quotation & ASCII.Quotation & "]" );
  s1(1) := "\";
  arrays.to_json( js, s1 );
  pragma assert( js = "[" & ASCII.Quotation & "\\" & ASCII.Quotation & "]" );
  s1(1) := "/";
  arrays.to_json( js, s1 );
  pragma assert( js = "[" & ASCII.Quotation & "\/" & ASCII.Quotation & "]" );
  s1(1) := "" & ASCII.BS;
  arrays.to_json( js, s1 );
  pragma assert( js = "[" & ASCII.Quotation & "\b" & ASCII.Quotation & "]" );
  s1(1) := "" & ASCII.FF;
  arrays.to_json( js, s1 );
  pragma assert( js = "[" & ASCII.Quotation & "\f" & ASCII.Quotation & "]" );
  s1(1) := "" & ASCII.LF;
  arrays.to_json( js, s1 );
  pragma assert( js = "[" & ASCII.Quotation & "\n" & ASCII.Quotation & "]" );
  s1(1) := "" & ASCII.CR;
  arrays.to_json( js, s1 );
  pragma assert( js = "[" & ASCII.Quotation & "\r" & ASCII.Quotation & "]" );
  s1(1) := "" & ASCII.HT;
  arrays.to_json( js, s1 );
  pragma assert( js = "[" & ASCII.Quotation & "\t" & ASCII.Quotation & "]" );
  --
  arrays.to_array( s1, "[" & ASCII.Quotation & "\" & ASCII.Quotation & ASCII.Quotation & "]" );
  pragma assert( s1(1) = "" & ASCII.Quotation );
  arrays.to_array( s1, "[" & ASCII.Quotation & "\\" & ASCII.Quotation & "]" );
  pragma assert( s1(1) = "\" );
  arrays.to_array( s1, "[" & ASCII.Quotation & "\/" & ASCII.Quotation & "]" );
  pragma assert( s1(1) = "/" );
  arrays.to_array( s1, "[" & ASCII.Quotation & "\b" & ASCII.Quotation & "]" );
  pragma assert( s1(1) = "" & ASCII.BS );
  arrays.to_array( s1, "[" & ASCII.Quotation & "\f" & ASCII.Quotation & "]" );
  pragma assert( s1(1) = "" & ASCII.FF );
  arrays.to_array( s1, "[" & ASCII.Quotation & "\n" & ASCII.Quotation & "]" );
  pragma assert( s1(1) = "" & ASCII.LF );
  arrays.to_array( s1, "[" & ASCII.Quotation & "\r" & ASCII.Quotation & "]" );
  pragma assert( s1(1) = "" & ASCII.CR );
  arrays.to_array( s1, "[" & ASCII.Quotation & "\t" & ASCII.Quotation & "]" );
  pragma assert( s1(1) = "" & ASCII.HT );
  --
  js := strings.to_json( "a" & ASCII.Quotation );
  pragma assert( js = ASCII.Quotation & "a\" & ASCII.Quotation & ASCII.Quotation );
  js := strings.to_json( "b\" );
  pragma assert( js = ASCII.Quotation & "b\\" & ASCII.Quotation );
  js := strings.to_json( "c/" );
  pragma assert( js = ASCII.Quotation & "c\/" & ASCII.Quotation );
  js := strings.to_json( "d" & ASCII.BS );
  pragma assert( js = ASCII.Quotation & "d\b" & ASCII.Quotation );
  js := strings.to_json( "e" & ASCII.FF );
  pragma assert( js = ASCII.Quotation & "e\f" & ASCII.Quotation );
  js := strings.to_json( "f" & ASCII.LF );
  pragma assert( js = ASCII.Quotation & "f\n" & ASCII.Quotation );
  js := strings.to_json( "g" & ASCII.CR );
  pragma assert( js = ASCII.Quotation & "g\r" & ASCII.Quotation );
  js := strings.to_json( "h" & ASCII.HT );
  pragma assert( js = ASCII.Quotation & "h\t" & ASCII.Quotation );
  --
  s := strings.to_string( json_string( ASCII.Quotation & "a\" & ASCII.Quotation & ASCII.Quotation ) );
  pragma assert( s = "a" & ASCII.Quotation );
  s := strings.to_string( json_string( ASCII.Quotation & "b\\" & ASCII.Quotation ) );
  pragma assert( s = "b\" );
  s := strings.to_string( json_string( ASCII.Quotation & "c\/" & ASCII.Quotation ) );
  pragma assert( s = "c/" );
  s := strings.to_string( json_string( ASCII.Quotation & "d\b" & ASCII.Quotation ) );
  pragma assert( s = "d" & ASCII.BS );
  s := strings.to_string( json_string( ASCII.Quotation & "e\f" & ASCII.Quotation ) );
  pragma assert( s = "e" & ASCII.FF );
  s := strings.to_string( json_string( ASCII.Quotation & "f\n" & ASCII.Quotation ) );
  pragma assert( s = "f" & ASCII.LF );
  s := strings.to_string( json_string( ASCII.Quotation & "g\r" & ASCII.Quotation ) );
  pragma assert( s = "g" & ASCII.CR );
  s := strings.to_string( json_string( ASCII.Quotation & "h\t" & ASCII.Quotation ) );
  pragma assert( s = "h" & ASCII.HT );

  js := "[" & ASCII.Quotation & "this" & ASCII.Quotation & "]";

  arrays.to_array( ja, js );

  pragma assert( ja(1) = "this" );

  ---

  js := "[123]";

  arrays.to_array( ja2, js );

  pragma assert( ja2(1) = 123 );

  ---

  js := "[true]";
  arrays.to_array( ja3, js );
  pragma assert( ja3(1) = true );

  js := "[false]";
  arrays.to_array( ja3, js );
  pragma assert( ja3(1) = false );

  -- Ensure subtype deref works

  js := "[true]";
  arrays.to_array( ja4, js );
  pragma assert( ja4(1) = true );

  js := "[false]";
  arrays.to_array( ja4, js );
  pragma assert( ja4(1) = false );

  -- Non-boolean enumerated array

  js := "[1]";
  arrays.to_array( ja5, js );
  pragma assert( ja5(1) = jte2 );

  js := "[2]";
  arrays.to_array( ja5, js );
  pragma assert( ja5(1) = jte3 );

  -- Arrays to Json

  ja(1) := "that";
  arrays.to_json( js, ja );
  pragma assert( js = "[" & ASCII.Quotation & "that" & ASCII.Quotation & "]" );

  ja2(1) := 123;
  arrays.to_json( js, ja2 );
  pragma assert( js = "[123]" );

  ja3(1) := false;
  arrays.to_json( js, ja3 );
  pragma assert( js = "[false]" );

  ja3(1) := true;
  arrays.to_json( js, ja3 );
  pragma assert( js = "[true]" );

  ja4(1) := false;
  arrays.to_json( js, ja4 );
  pragma assert( js = "[false]" );

  ja4(1) := true;
  arrays.to_json( js, ja4 );
  pragma assert( js = "[true]" );

  ja5(1) := jte1;
  arrays.to_json( js, ja5 );
  pragma assert( js = "[0]" );

  -- Json to Records

  js := "{" &
       ASCII.Quotation & "s" & ASCII.Quotation &
       ":" &
       ASCII.Quotation & "this" & ASCII.Quotation &
       "}";
  records.to_record( rs, js );
  pragma assert( rs.s = "this" );

  js := "{" &
       ASCII.Quotation & "s" & ASCII.Quotation &
       ":" &
       ASCII.Quotation & "\" & ASCII.Quotation & "this" & ASCII.Quotation &
       "}";
  records.to_record( rs, js );
  pragma assert( rs.s = ASCII.Quotation & "this" );

  js := "{" &
       ASCII.Quotation & "s" & ASCII.Quotation &
       ":" &
       ASCII.Quotation & "this:" & ASCII.Quotation &
       "}";
  records.to_record( rs, js );
  pragma assert( rs.s = "this:" );

  js := "{" &
     ASCII.Quotation & "s" & ASCII.Quotation &
     ":" &
     ASCII.Quotation & "this," & ASCII.Quotation &
     "}";
  records.to_record( rs, js );
  pragma assert( rs.s = "this," );

  js := "{" &
       ASCII.Quotation & "s" & ASCII.Quotation &
       ":" &
       ASCII.Quotation & "\" & ASCII.Quotation & "this," & ASCII.Quotation &
       "}";
  records.to_record( rs, js );
  pragma assert( rs.s = ASCII.Quotation & "this," );

  js := "{" &
       ASCII.Quotation & "s" & ASCII.Quotation &
       ":" &
       ASCII.Quotation & "this" & ASCII.Quotation &
       "," &
       ASCII.Quotation & "n" & ASCII.Quotation &
       ":5}";
  records.to_record( rs2, js );
  pragma assert( rs2.s = "this" );
  pragma assert( rs2.n = 5 );

  js := "{" &
       ASCII.Quotation & "n" & ASCII.Quotation &
       ":6," &
       ASCII.Quotation & "s" & ASCII.Quotation &
       ":" &
       ASCII.Quotation & "that" & ASCII.Quotation &
       "}";
  records.to_record( rs2, js );
  pragma assert( rs2.s = "that" );
  pragma assert( rs2.n = 6 );

  rs3.isTrue := true;
  rs3.isFalse := false;
  js := "{" &
       ASCII.Quotation & "isTrue" & ASCII.Quotation &
       ":true," &
       ASCII.Quotation & "isFalse" & ASCII.Quotation &
       ":false}";
  records.to_record( rs3, js );
  pragma assert( rs3.isTrue = true );
  pragma assert( rs3.isFalse = false );

  rs4.enum := jte1;
  js := "{" &
       ASCII.Quotation & "enum" & ASCII.Quotation &
       ":1}";
  records.to_record( rs4, js );
  pragma assert( rs4.enum = jte2 );

  js := "{" &
       ASCII.Quotation & "isTrue" & ASCII.Quotation &
       ":true," &
       ASCII.Quotation & "isFalse" & ASCII.Quotation &
       ":false}";

  records.to_record( rs5, js );
  pragma assert( rs5.isTrue = true );
  pragma assert( rs5.isFalse = false );

  -- Records to Json

  rs.s := "code browser";
  records.to_json( js, rs );
  pragma assert( js = "{" & ASCII.Quotation & "s" & ASCII.Quotation &
         ":" & ASCII.Quotation & "code browser" & ASCII.Quotation & "}" );

  rs2.s := "wet sock";
  rs2.n := 15;
  records.to_json( js, rs2 );
  pragma assert( js = "{" & ASCII.Quotation & "s" & ASCII.Quotation &
         ":" & ASCII.Quotation & "wet sock" & ASCII.Quotation &
         "," & ASCII.Quotation & "n" & ASCII.Quotation &
         ": 15}" );

  rs3.isTrue := true;
  rs3.isFalse := false;
  records.to_json( js, rs3 );
  pragma assert( js = "{" & ASCII.Quotation & "isTrue" & ASCII.Quotation &
         ":true," & ASCII.Quotation & "isFalse" & ASCII.Quotation &
         ":false}" );

  rs4.enum := jte2;
  records.to_json( js, rs4 );
  pragma assert( js = "{" & ASCII.Quotation & "enum" & ASCII.Quotation &
         ": 1}" );

  rs5.isTrue := true;
  rs5.isFalse := false;
  records.to_json( js, rs5 );
  pragma assert( js = "{" & ASCII.Quotation & "isTrue" & ASCII.Quotation &
         ":true," & ASCII.Quotation & "isFalse" & ASCII.Quotation &
         ":false}" );

  ja6(1) := "[1,2]";
  ja6(2) := "[3,4]";
  arrays.to_json( js, ja6 );
  pragma assert( js = "[[1,2],[3,4]]" );

  rs6.i := 0;
  rs6.a := "[1,2]";
  records.to_json( js, rs6 );
  pragma assert( js = "{" &
    ASCII.Quotation & "i" & ASCII.Quotation & ": 0," &
    ASCII.Quotation & "a" & ASCII.Quotation & ":[1,2]}" );

  ja7(1) := "{" & ASCII.Quotation & "i" & ASCII.Quotation & ": 0}";
  ja7(2) := "[1,2]";
  arrays.to_json( js, ja7 );
  pragma assert( js = "[{" & ASCII.Quotation &
  "i" & ASCII.Quotation & ": 0},[1,2]]" );

  js := "[{" &'"' & 'i' & '"' & ":0},[2,3]]";
  arrays.to_array( ja8, js );
  pragma assert( ja8(1) = "{" &'"' & 'i' & '"' & ":0}" );
  pragma assert( ja8(2) = "[2,3]" );

  js := "{" & '"' & "i" & '"' & ":4," &
     '"' & "s" & '"' & ":" & '"' & "hello" & '"' & "," &
    '"' & "j" & '"' & ":[1,2,3]," &
    '"' & "k" & '"' & ":{" & '"' & "x" & '"' & ":0}}";
  records.to_record( rs7, js );
  pragma assert( rs7.i = 4 );
  pragma assert( rs7.s = "hello" );
  pragma assert( rs7.j = "[1,2,3]" );
  pragma assert( rs7.k = "{" & '"' & "x" & '"' & ":0}" );

  js := "{" & '"' & "j" & '"' & ":[1,2,3]" & "}";
  records.to_record( rs8, js );
  pragma assert( rs8.j = "[1,2,3]" );

end;

-- if statements

if i = 1 then
  null;
end if;

if i = 1 then
   null;
elsif i = 2 then
   null;
end if;

if i = 1 then
   null;
elsif i = 2 then
   null;
elsif i = 3 then
   null;
end if;

if i = 1 then
   null;
else
   null;
end if;

if i = 1 then
   null;
elsif i = 2 then
   null;
else
   null;
end if;

-- case statements

i := 1;
i2 := 0;
case i is
when 1 => i2 := 1;
when others => i2 :=2;
end case;
pragma assert( i2 = 1 );

i := 2;
i2 := 0;
case i is
when 1 => i2 := 1;
when others => i2 :=2;
end case;
pragma assert( i2 = 2 );

s := "a";
i2 := 0;
case s is
when "a" => i2 := 1;
when others => i2 := 2;
end case;
pragma assert( i2 = 1 );

s := "b";
i2 := 0;
case s is
when "a" => i2 := 1;
when others => i2 := 2;
end case;
pragma assert( i2 = 2 );

type case_enum is ( caseenum1, caseenum2 );

cenum : case_enum := caseenum1;
i2 := 0;
case cenum is
when caseenum1 => i2 := 1;
when others => i2 := 2;
end case;
pragma assert( i2 = 1 );

cenum := caseenum2;
i2 := 0;
case cenum is
when caseenum1 => i2 := 1;
when others => i2 := 2;
end case;
pragma assert( i2 = 2 );

i2 := 0;
c := 'a';
case c is
when 'a' =>
  i2 := 1;
when others =>
  i2 := 2;
end case;
pragma assert( i2 = 1 );

s := "a";
i2 := 0;
case s is
when "a"|"b" => i2 := 1;
when others => i2 := 2;
end case;
pragma assert( i2 = 1 );

s := "b";
i2 := 0;
case s is
when "a"|"b" => i2 := 1;
when others => i2 := 2;
end case;
pragma assert( i2 = 1 );

s := "c";
i2 := 0;
case s is
when "a"|"b" => i2 := 1;
when others => i2 := 2;
end case;
pragma assert( i2 = 2 );

-- while loop and exit statement

i := 1;
while i /= 1 loop
  null;
end loop;
pragma assert( i=1 ); -- while 1

i := 1;
while i < 5 loop
   i := @+1;
end loop;
pragma assert( i=5 ); -- while 2

i := 1;
while i < 5 loop
   exit;
end loop;
pragma assert( i=1 ); -- while 3

i := 1;
while i < 5 loop
   i := @+1;
   exit when i = 3;
end loop;
pragma assert( i=3 ); -- while 4

i := 1;
while i < 5 loop
   i := @+1;
   if i = 3 then
      exit;
   end if;
end loop;
pragma assert( i=3 ); -- while 5

i := 1;
while i < 5 loop
   if i = 3 then
      exit;
   end if;
   i := @+1;
end loop;
pragma assert( i=3 ); -- while 6

i := 1;
while i < 5 loop i := @+1; end loop;
pragma assert( i = 5 );

-- while block skipping/exit tests

i := 1;
if false then
   while i < 3 loop
      i := @+1;
   end loop;
end if;
pragma assert( i = 1 );

i := 1;
if true then
   while i < 3 loop
      i := 2;
      exit;
   end loop;
   while i < 3 loop
      i := 3;
      exit;
   end loop;
end if;
pragma assert( i = 3 );

i := 1;
if false then
   while i < 3 loop
      i := 2;
      exit;
   end loop;
   while i < 3 loop
      i := 3;
      exit;
   end loop;
end if;
pragma assert( i = 1 );

i := 1;
while i < 3 loop
  i := 2;
  exit when true;
  while i < 3 loop
    i := 3;
    exit;
  end loop;
end loop;
pragma assert( i = 2 );

-- for loop and itself operand

i := 99;
un := 0;
for i in 1..10 loop
    un := @+1;
end loop;
pragma assert( i=99 ); -- first for
pragma assert( un=10 ); -- first for

i := 99;
un := 0;
for i in reverse 1..10 loop
    un := @+1;
end loop;
pragma assert( i=99 ); -- second for
pragma assert( un=10 ); -- second for

i := 0;
type enum_for is (enumf1, enumf2, enumf3 );
for e in enumf1..enumf3 loop
  i := @+1;
end loop;
pragma assert( i = 3 );

i := 0;
for e in reverse enumf1..enumf3 loop
  i := @+1;
end loop;
pragma assert( i = 3 );

un := 0;
for i in 1..5 loop un := @+1; end loop;
pragma assert( un = 5 );

-- for block skipping tests

i := 1;
if false then
   for j in 1..5 loop
      i := 2;
   end loop;
end if;
pragma assert( i = 1 );

i := 1;
if true then
   for j in 1..5 loop
      i := 2;
      exit;
   end loop;
   for j in 1..5 loop
      i := 3;
      exit;
   end loop;
end if;
pragma assert( i = 3 );

i := 1;
if false then
   for j in 1..5 loop
      i := 2;
      exit;
   end loop;
   for j in 1..5 loop
      i := 3;
      exit;
   end loop;
end if;
pragma assert( i = 1 );

i := 1;
for j in 1..5 loop
  i := 2;
  exit when true;
  for j in 1..5 loop
    i := 3;
    exit;
  end loop;
end loop;
pragma assert( i = 2 );

-- loop loop

i := 0;
loop
  i := @+1;
  exit when i>9;
end loop;
pragma assert( i = 10 );

i := 0;
loop i := @+1; exit when i>9; end loop;
pragma assert( i = 10 );

i := 1;
if false then
   loop
      i := 2;
      exit;
   end loop;
end if;
pragma assert( i = 1 );

i := 1;
if true then
   loop
      i := 2;
      exit;
   end loop;
   loop
      i := 3;
      exit;
   end loop;
end if;
pragma assert( i = 3 );

i := 1;
if false then
   loop
      i := 2;
      exit;
   end loop;
   loop
      i := 3;
      exit;
   end loop;
end if;
pragma assert( i = 1 );

i := 1;
loop
  i := 2;
  exit when true;
  loop
    i := 3;
    exit;
 end loop;
end loop;
pragma assert( i = 2 );

-- O/S commands

echo;
"echo";
echo ";";
echo "|";
echo \; ;
echo \" ;
echo \' ;
echo \\ ;
echo "'";
echo '"';
echo '\';
echo is;
-- ^ compressed token ('is' is a reserved word)
-- command echo;

-- Path globbing tests

touch /tmp/a;
rm /tmp/a;
touch /tmp/foo123;
ls /tmp/foo123;
ls /tmp/foo1*3;
ls /t*p/foo1*3;
rm /tmp/foo123;
touch ./foo123;
rm ./foo123;
ls *goodte*t.sp;
mkdir /tmp/z;
touch /tmp/z/b;
ls /tmp/z/*;
rm /tmp/z/b;
rmdir /tmp/z;

touch ./__testfile;
rm ./__testfile;
touch "./__testfile";
rm "./__testfile";
touch './__testfile';
rm './__testfile';
touch ("./__testfile");
rm ("./__testfile");
sleep (1) &;
wait;
sleep 1 &;
wait;
cmd_echo : limited command := "/bin/echo";
cmd_echo;

tmp : limited string := "t.tmp";

-- Bourne Shell Compatibility: Standard Output

-- Literal target

echo "a" > t.tmp;
result := `cat t.tmp`;
pragma assert( result = "a" );
rm t.tmp;

echo "b" >t.tmp;
result := `cat t.tmp`;
pragma assert( result = "b" );
rm t.tmp;

-- Double quoted target

echo "c" > "t.tmp";
result := `cat t.tmp`;
pragma assert( result = "c" );
rm t.tmp;

echo "d" > "$tmp";
result := `cat t.tmp`;
pragma assert( result = "d" );
rm t.tmp;

echo "e" >"$tmp";
result := `cat t.tmp`;
pragma assert( result = "e" );
rm t.tmp;

echo "f">"t.tmp";
result := `cat t.tmp`;
pragma assert( result = "f" );
rm t.tmp;

echo "f">"t.tmp";
result := `cat t.tmp`;
pragma assert( result = "f" );
rm t.tmp;

echo ">" > "t.tmp";
result := `cat t.tmp`;
pragma assert( result = ">" );
rm t.tmp;

-- Single quoted target

echo "g" > 't.tmp';
result := `cat t.tmp`;
pragma assert( result = "g" );
rm t.tmp;

echo "h" > '$tmp';
result := `cat '$tmp'`;
pragma assert( result = "h" );
rm '$tmp';

echo "i" >'$tmp';
result := `cat '$tmp'`;
pragma assert( result = "i" );
rm '$tmp';

echo 'j'>'t.tmp';
result := `cat t.tmp`;
pragma assert( result = "j" );
rm t.tmp;

echo '>' > "t.tmp";
result := `cat t.tmp`;
pragma assert( result = ">" );
rm t.tmp;

-- Bourne Shell Compatibility: Standard Output Append

-- Literal target

touch t.tmp;
echo "a1" >> t.tmp;
result := `cat t.tmp`;
pragma assert( result = "a1" );
rm t.tmp;

touch t.tmp;
echo "b2" >>t.tmp;
result := `cat t.tmp`;
pragma assert( result = "b2" );
rm t.tmp;

-- Double quoted target

touch t.tmp;
echo "c2" >> "t.tmp";
result := `cat t.tmp`;
pragma assert( result = "c2" );
rm t.tmp;

touch t.tmp;
echo "d2" >> "$tmp";
result := `cat t.tmp`;
pragma assert( result = "d2" );
rm t.tmp;

touch t.tmp;
echo "e2" >>"$tmp";
result := `cat t.tmp`;
pragma assert( result = "e2" );
rm t.tmp;

touch t.tmp;
echo "f2">>t.tmp;
result := `cat t.tmp`;
pragma assert( result = "f2" );
rm t.tmp;

echo ">>" > "t.tmp";
result := `cat t.tmp`;
pragma assert( result = ">>" );
rm t.tmp;

-- Single quoted target

touch t.tmp;
echo "g2" >> 't.tmp';
result := `cat t.tmp`;
pragma assert( result = "g2" );
rm t.tmp;

touch '$tmp';
echo "h2" >> '$tmp';
result := `cat '$tmp'`;
pragma assert( result = "h2" );
rm '$tmp';

touch '$tmp';
echo "i2" >>'$tmp';
result := `cat '$tmp'`;
pragma assert( result = "i2" );
rm '$tmp';

touch t.tmp;
echo 'j2'>>'t.tmp';
result := `cat t.tmp`;
pragma assert( result = "j2" );
rm t.tmp;

echo '>>' > "t.tmp";
result := `cat t.tmp`;
pragma assert( result = ">>" );
rm t.tmp;

-- Bourne Shell Compatibility: Standard Error Output

-- Literal Target

echo "a" 2> t.tmp;
result := `cat < t.tmp`;
-- trace output will add output to the next line
pragma assert( result /= "a" );
rm t.tmp;

echo "b" 2>t.tmp;
result := `cat < t.tmp`;
-- trace output will add output to the next line
pragma assert( result /= "b" );
rm t.tmp;

echo "c" 21 > t.tmp;
result := `cat < t.tmp`;
pragma assert( result = "c 21" );
rm t.tmp;

-- Double quoted target

echo "c" > t.tmp;
echo "c2" 2> "t.tmp";
result := `cat t.tmp`;
pragma assert( result /= "c2" );
rm t.tmp;

echo "d" > t.tmp;
echo "d2" 2> "$tmp";
result := `cat t.tmp`;
pragma assert( result /= "d2" );
rm t.tmp;

echo "e" > t.tmp;
echo "e2" 2>"$tmp";
result := `cat t.tmp`;
pragma assert( result /= "e2" );
rm t.tmp;

echo "2>" > "t.tmp";
result := `cat t.tmp`;
pragma assert( result = "2>" );
rm t.tmp;

-- Single quoted target

echo "g" > t.tmp;
echo "g2" 2> 't.tmp';
result := `cat t.tmp`;
pragma assert( result /= "g2" );
rm t.tmp;

echo "h" > t.tmp;
echo "h2" 2> '$tmp';
result := `cat '$tmp'`;
pragma assert( result /= "h2" );
rm '$tmp';

echo "i" > t.tmp;
echo "i2" 2>'$tmp';
result := `cat '$tmp'`;
pragma assert( result /= "i2" );
rm '$tmp';

echo "j" > t.tmp;
echo 'j2'2>'t.tmp';
result := `cat t.tmp`;
pragma assert( result /= "j2" );
rm t.tmp;

echo '2>' > "t.tmp";
result := `cat t.tmp`;
pragma assert( result = "2>" );
rm t.tmp;

-- Bourne Shell Compatibility: Standard Error Output Append

-- Literal Target

touch t.tmp;
echo "a" 2>> t.tmp;
result := `cat < t.tmp`;
-- trace output will add output to the next line
pragma assert( result /= "a" );
rm t.tmp;

touch t.tmp;
echo "b" 2>>t.tmp;
result := `cat < t.tmp`;
-- trace output will add output to the next line
pragma assert( result /= "b" );
rm t.tmp;

touch t.tmp;
echo "c" 21 >> t.tmp;
result := `cat < t.tmp`;
pragma assert( result = "c 21" );
rm t.tmp;

-- Double quoted target

echo "c" > t.tmp;
echo "c2" 2>> "t.tmp";
result := `cat t.tmp`;
pragma assert( result /= "c2" );
rm t.tmp;

echo "d" > t.tmp;
echo "d2" 2>> "$tmp";
result := `cat t.tmp`;
pragma assert( result /= "d2" );
rm t.tmp;

echo "e" > t.tmp;
echo "e2" 2>>"$tmp";
result := `cat t.tmp`;
pragma assert( result /= "e2" );
rm t.tmp;

echo "2>>" > "t.tmp";
result := `cat t.tmp`;
pragma assert( result = "2>>" );
rm t.tmp;

-- Single quoted target

echo "g" > t.tmp;
echo "g2" 2>> 't.tmp';
result := `cat t.tmp`;
pragma assert( result /= "g2" );
rm t.tmp;

echo "h" > '$tmp';
echo "h2" 2>> '$tmp';
result := `cat '$tmp'`;
pragma assert( result /= "h2" );
rm '$tmp';

echo "i" > '$tmp';
echo "i2" 2>>'$tmp';
result := `cat '$tmp'`;
pragma assert( result /= "i2" );
rm '$tmp';

echo "j" > 't.tmp';
echo 'j2'2>>'t.tmp';
result := `cat t.tmp`;
pragma assert( result /= "j2" );
rm t.tmp;

echo '2>>' > "t.tmp";
result := `cat t.tmp`;
pragma assert( result = "2>>" );
rm t.tmp;

-- Bourne Shell Compatibility: Standard Input

-- Literal target

echo "a" > t.tmp;
result := `cat < t.tmp`;
pragma assert( result = "a" );
rm t.tmp;

echo "b" > t.tmp;
result := `cat <t.tmp`;
? result;
pragma assert( result = "b" );
rm t.tmp;

-- Double quoted target

echo "a" > "t.tmp";
result := `cat < "$tmp";`;
pragma assert( result = "a" );
rm t.tmp;

echo "b" > "t.tmp";
result := `cat <"$tmp";`;
pragma assert( result = "b" );
rm t.tmp;

echo "<" > "t.tmp";
result := `cat t.tmp`;
pragma assert( result = "<" );
rm t.tmp;

echo "a" > '$tmp';
result := `cat < '$tmp';`;
pragma assert( result = "a" );
rm '$tmp';

-- Single quoted target

echo '<' > "t.tmp";
result := `cat t.tmp`;
pragma assert( result = "<" );
rm t.tmp;

echo "b" > '$tmp';
result := `cat <'$tmp';`;
pragma assert( result = "b" );
rm '$tmp';

-- Pipelines

echo | head;
echo | sort | head;
echo "test" | head;
echo ( "test" ) | head;
-- Piping standard error in a pipeline
s := `tr -z 2>&1 | wc -c;`;
i := numerics.value( s );
pragma assert( i > 0 );
s := `echo | tr -z 2>&1 | wc -c;`;
i := numerics.value( s );
pragma assert( i > 0 );

-- 2> redirection
tr -z 2> "t.tmp.$$";
s := `cat "t.tmp.$$";`;
pragma assert( s /= "" );
rm "t.tmp.$$";

tr -z 2> "t.tmp.$$" | cat;
s := `cat "t.tmp.$$";`;
pragma assert( s /= "" );
rm "t.tmp.$$";

echo | tr -z 2> "t.tmp.$$" | cat;
s := `cat "t.tmp.$$";`;
pragma assert( s /= "" );
rm "t.tmp.$$";

echo `echo \`` > "t.tmp.$$";
s := `cat "t.tmp.$$"`;
rm "t.tmp.$$";
pragma assert( s = '`' );

-- character escaping tests

echo h;
echo hello;
echo hello hello;
echo '*';
echo "*";
echo *;
echo "'";
echo '"';
echo "''";
echo '""';
echo \*;
echo '*';
echo '\*';
echo \\;
echo \\\\;
echo "[]";
echo [];
-- escape globbing characters
s := "*[\?";
s := `echo "$s";`;
pragma assert( s = "*[\?" );

s := `echo '*[\?';`;
pragma assert( s = "*[\?" );

s := `echo "\$\"\`\\";`;
pragma assert( s = "$" & ASCII.Quotation & "`\" );
s := `echo "\$\"\\";`;
pragma assert( s = "$" & ASCII.Quotation & "\" );

-- Shell Variable Substitution Tests

-- without suppression

-- with suppression

pragma suppress( word_quoting );

i := 1;
s := `echo "$i";`;
pragma assert( s = "1" );
i := 2;
s := `echo "$i";`;
pragma assert( s = "2" );
i := 0;
s := `echo "$i;";`;
pragma assert( s = "0;" );
i := 3;
s := `echo '$i';`;
pragma assert( s = "$i" );
i := 4;
s := `echo $i"h";`;
pragma assert( s = "4h" );
i := 5;
s := `echo $i'j';`;
pragma assert( s = "5j" );
i := 6;
s := `echo "$i"k;`;
pragma assert( s = "6k" );
i := 7;
s := `echo "$i"'l';`;
pragma assert( s = "7l" );
i := 8;
s := `echo "$i""m";`;
pragma assert( s = "8m" );
i := 9;
s := `echo n"$i";`;
pragma assert( s = "n9" );
i := 0;
s := `echo "$i$i";`;
pragma assert( s = "00" );
-- Space Handling in Double Quotes
s1 := " h ";
s := `echo $s1;`;
pragma assert( s = "h" );
s1 := " h ";
s := `echo " $s1 ";`;
s1 := "-n c";
-- bareword
s := `echo $s1;`;
pragma assert( s = "c" );
-- double quotes
s := `echo "$s1";`;
pragma assert( s = "-n c" );
-- single quotes
s := `echo '$s1';`;
? s;
pragma assert( s = "$s1" );

-- Shell Globbing Tests

s := `echo \*;`;
pragma assert( s = "*" );
s := `echo "*";`;
pragma assert( s = "*" );
s := `echo '*';`;
pragma assert( s = "*" );
s := "*";
s := `echo "$s";`;
pragma assert( s = "*" );
s := "*";
s := `echo '$s';`;
pragma assert( s = "$s" );
s := "*";
s := `echo $s;`;
pragma assert( s /= "*" );

-- Special Substitutions (Dollar Expansions)

s := `echo $?;`; -- status
s := `echo $#;`; -- number of arguments
pragma assert( s = "3" );
s := `echo $$;`; -- pid
s := `echo $0;`; -- command name
s := `echo $1;`; -- first argument
pragma assert( s = "a" );
s := `echo $!;`; -- last pid

-- tilde expansion

s  := `echo ~;`;
s1 := `echo $HOME;`;
pragma assert( s = s1 );

s  := `echo /~;`;
pragma assert( s = "/~" );

s1 := "~";
s  := `echo $s1;`;
pragma assert( s = "~" );

s  := `echo \~;`;
pragma assert( s = "~" );

s  := `echo '~';`;
pragma assert( s = "~" );

s  := `echo "~";`;
pragma assert( s = "~" );

s  := `echo ~ken;`;
pragma assert( s = "/home/ken" );

s  := `echo "~ken";`;
pragma assert( s = "~ken" );

s  := `echo ken~;`;
pragma assert( s = "ken~" );

s  := "~/ken";
s  := `echo "$s";`;
pragma assert( s = "~/ken" );

-- New features of shell rewrite: brace substitutions

s  := `echo ${HOME}`;
pragma assert( s = HOME );

s  := `echo "${HOME}"`;
pragma assert( s = HOME );

s  := `echo '${HOME}'`;
pragma assert( s = "${HOME}" );

s  := `echo ${#HOME}`;
pragma assert( numerics.value(s) = strings.length( HOME ) );

s  := `echo ${0}`;
pragma assert( s = command_line.command_name );

s  := `echo ${#0}`;
pragma assert( numerics.value(s) = strings.length( command_line.command_name ) );

s  := `echo ${1}`;
pragma assert( s = command_line.argument(1) );

s  := `echo ${#1}`;
pragma assert( numerics.value(s) = strings.length( command_line.argument(1) ) );

s  := `echo ${#}`;
pragma assert( numerics.value(s) = command_line.argument_count );

s  := `echo ${?}`;
pragma assert( numerics.value(s) = os.status );

s  := `echo ${$}`;
pragma assert( numerics.value(s) = os.pid );

s  := `echo ${!}`;
pragma assert( numerics.value(s) = os.last_child );

s  := `echo ${true}`;
pragma assert( s = "true" );

b  := true;
s  := `echo ${b}`;
pragma assert( s = "true" );

-- Dollar Brace Expansion: Double Quotes

echo "${0}" > t.tmp.$$;
result := `cat t.tmp.$$`;
pragma assert( $0 = result );
rm t.tmp.$$;

echo "${#}" > t.tmp.$$;
result := `cat t.tmp.$$`;
pragma assert( $# = numerics.value(result) );
rm t.tmp.$$;

echo "${!}" > t.tmp.$$;
result := `cat t.tmp.$$`;
pragma assert( $! = numerics.value(result) );
rm t.tmp.$$;

echo "${?}" > t.tmp.$$;
result := `cat t.tmp.$$`;
pragma assert( "0" = result );
rm t.tmp.$$;

echo "${$}" > t.tmp.$$;
result := `cat t.tmp.$$`;
pragma assert( $$ = numerics.value(result) );
rm t.tmp.$$;

echo "${HOME}" > t.tmp.$$;
result := `cat t.tmp.$$`;
pragma assert( HOME = result );
rm t.tmp.$$;

-- Dollar Brace Expansion: Combinations

echo ${HOME}${HOME} > "t.tmp.$$";
result := `cat "t.tmp.$$"`;
pragma assert( HOME & HOME = result );
rm "t.tmp.$$";

echo "${HOME}${HOME}" > "t.tmp.$$";
result := `cat "t.tmp.$$"`;
pragma assert( HOME & HOME = result );

echo "`pwd`" > "t.tmp.$$" ;
s := `cat "t.tmp.$$"`;
rm "t.tmp.$$" ;
pragma assert( s = `pwd` );

s  := "foo";
s1 := `echo ${s:-bar}`;
pragma assert( s1 = "foo" );

s1 := `echo ${s:+bar}`;
pragma assert( s1 = "bar" );

s  := "";
s1 := `echo ${s:-bar}`;
pragma assert( s1 = "bar" );

s1 := `echo ${s:+bar}`;
pragma assert( s1 = "" );

s  := "";
s1 := `echo ${s:-}`;
pragma assert( s1 = "" );

s  := "abc";
s1 := `echo ${s:+}`;
pragma assert( s1 = "" );

-- New features of shell rewrite: brace process substitutions

s := `echo $(pwd)`;
pragma assert( s = `pwd` );

-- No commands does nothing in Bourne shell

s := `echo $()`;
pragma assert( s = "" );

-- Recursion allowed

--s := `echo $(echo $(pwd))`;
--pragma assert( s = `pwd` );

-- pathname expansion with a single / as directory

s := `echo /tmp ;`;
pragma assert( s = "/tmp" );

-- advanced text_io

create( ft, out_file, "./__testfile" );
close( ft );

open( ft, in_file, "./__testfile" );
b := end_of_file( ft );
pragma assert( b = true );
b := is_open( ft );
pragma assert( b = true );
close( ft );

open( ft, out_file, "./__testfile" );
put_line( ft, "a test line" );
close( ft );

open( ft, in_file, "./__testfile" );
b := end_of_file( ft );
pragma assert( b = false );
s := name( ft );
pragma assert( s = "./__testfile" );
fm := mode( ft );
pragma assert( fm = in_file );
i := line( ft );
pragma assert( i = 0 );
skip_line( ft );
i := line( ft );
pragma assert( i = 2 ); -- 1 + EOF
b := end_of_file( ft );
pragma assert( b = true );
close( ft );

open( ft, in_file, "./__testfile" );
b := end_of_file( ft );
pragma assert( b = false );
s := name( ft );
pragma assert( s = "./__testfile" );
fm := mode( ft );
pragma assert( fm = in_file );
i := line( ft );
pragma assert( i = 0 );
s := get_line( ft );
pragma assert( s = "a test line" );
i := line( ft );
pragma assert( i = 2 ); -- 1 + EOF
b := end_of_file( ft );
pragma assert( b = true );
reset( ft );
fm := mode( ft );
pragma assert( fm = in_file );
get( ft, c );
pragma assert( c = 'a' );
b := end_of_line( ft );
pragma assert( b = false );
reset( ft, out_file );
fm := mode( ft );
pragma assert( fm = out_file );
delete( ft );

-- trace command

trace false;
trace;
trace true;
trace;

i_iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii : integer; -- a very long name

i_iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii := 0; -- to avoid unused var warning

-- output redirection

create( ft, out_file, "./test2.txt" );
set_output( ft );
pragma assert( mode( current_output ) = mode( ft ) );
pragma assert( name( current_output ) = name( ft ) );
put_line( ft, "testing" );
close( ft );
open( ft, append_file, "./test2.txt" );
set_output( ft );
pragma assert( mode( current_output ) = mode( ft ) );
pragma assert( name( current_output ) = name( ft ) );
set_output( standard_output );
close( ft );
pragma assert( mode( current_output ) = mode( standard_output ) );
pragma assert( name( current_output ) = name( standard_output ) );
set_output( standard_output );
pragma assert( mode( current_output ) = mode( standard_output ) );
pragma assert( name( current_output ) = name( standard_output ) );
set_output( current_output );
pragma assert( mode( current_output ) = mode( standard_output ) );
pragma assert( name( current_output ) = name( standard_output ) );

-- input redirection

open( ft, in_file, "./test2.txt" );
set_input( ft );
pragma assert( mode( current_input ) = mode( ft ) );
pragma assert( name( current_input ) = name( ft ) );
s := get_line( ft );
pragma assert( s = "testing" );
set_input( standard_input );
close( ft );
pragma assert( mode( current_input ) = mode( standard_input ) );
pragma assert( name( current_input ) = name( standard_input ) );
set_input( standard_input );
pragma assert( mode( current_input ) = mode( standard_input ) );
pragma assert( name( current_input ) = name( standard_input ) );
set_input( current_input );
pragma assert( mode( current_input ) = mode( standard_input ) );
pragma assert( name( current_input ) = name( standard_input ) );

-- error redirection

open( ft, out_file, "./test2.txt" );
set_error( ft );
pragma assert( mode( current_error ) = mode( ft ) );
pragma assert( name( current_error ) = name( ft ) );
set_error( standard_error );
close( ft );
open( ft, append_file, "./test2.txt" );
set_error( ft );
pragma assert( mode( current_error ) = mode( ft ) );
pragma assert( name( current_error ) = name( ft ) );
set_error( standard_error );
delete( ft );
pragma assert( mode( current_error ) = mode( standard_error ) );
pragma assert( name( current_error ) = name( standard_error ) );
set_error( standard_error );
pragma assert( mode( current_error ) = mode( standard_error ) );
pragma assert( name( current_error ) = name( standard_error ) );
set_error( current_error );
pragma assert( mode( current_error ) = mode( standard_error ) );
pragma assert( name( current_error ) = name( standard_error ) );

-- Calendar package

current_time : calendar.time := calendar.clock;
cy : calendar.year_number := calendar.year( current_time );
pragma assert( cy > 1900 and cy < 2200 );
cm : calendar.month_number := calendar.month( current_time );
pragma assert( cm > 0 and cm < 13 );
cd1: calendar.day_number := calendar.day( current_time );
pragma assert( cd1 > 0 and cd1 < 32 );
cs : calendar.day_duration := calendar.seconds( current_time );
pragma assert (cs > 0 and cs <= 86400 );
cy2: calendar.year_number;
cm2: calendar.month_number;
cd2: calendar.day_number;
cs2: calendar.day_duration;
calendar.split( current_time, cy2, cm2, cd2, cs2 );
pragma assert( cy = cy2 );
pragma assert( cm = cm2 );
pragma assert( cd1 = cd2 );
pragma assert( cs = cs2 );
another_time : calendar.time := calendar.time_of( cy, cm, cd1, cs );
calendar.split( another_time, cy, cm, cd1, cs );
pragma assert( cy = cy2 );
pragma assert( cm = cm2 );
pragma assert( cd1 = cd2 );
pragma assert( cs = cs2 );
another_time := calendar.time_of( 2016, 3, 15, 0 );
li := calendar.to_julian( another_time );
pragma assert( li = 2457463 );
another_time2 : calendar.time := calendar.to_time( li );
calendar.split( another_time2, cy, cm, cd1, cs );
pragma assert( cy = 2016 );
pragma assert( cm = 3 );
pragma assert( cd1 = 15 );
pragma assert( cs = 0 );
i := calendar.day_of_week( another_time );
pragma assert( i = 3 );
another_time := files.last_modified( "goodtest.sp" );
calendar.split( another_time, cy, cm, cd1, cs );
pragma assert( cy > 2000 ); -- test not exact
another_time := files.last_changed( "goodtest.sp" );
calendar.split( another_time, cy, cm, cd1, cs );
pragma assert( cy > 2000 ); -- test not exact
another_time := files.last_accessed( "goodtest.sp" );
calendar.split( another_time, cy, cm, cd1, cs );
pragma assert( cy > 2000 ); -- test not exact

-- calendar arithmetic

old_current_time : calendar.time := current_time;
current_time := current_time - 1.0;
current_time := current_time + 1.0;
b := current_time = old_current_time;
pragma assert( b = true );
-- Code coverage fails here due to previous error
current_time := 1.0 + current_time;
b := current_time > old_current_time;
pragma assert( b = true );
dur := current_time - old_current_time;
pragma assert( dur = 1.0 );
current_time := old_current_time;
b := current_time > current_time;
pragma assert( b = false );
b := current_time >= current_time;
pragma assert( b = true );
b := current_time < current_time;
pragma assert( b = false );
b := current_time <= current_time;
pragma assert( b = true );
b := current_time /= current_time;
pragma assert( b = false );
b := current_time in current_time..current_time;
pragma assert( b = true );
b := current_time not in current_time..current_time;
pragma assert( b = false );

-- CGI package

b := cgi.parsing_errors;
pragma assert( b = true );
b := cgi.input_received;
pragma assert( b = false );
b := cgi.is_index;
pragma assert( b = false );
cgim : cgi.cgi_method_type := cgi.cgi_method;
pragma assert( cgim = cgi.unknown );
s := cgi.value( "xyzabc_fake", 1 ); -- hopefully non-existent!
pragma assert( s = "" );
s := cgi.value( "xyzabc_fake", 1, false );
pragma assert( s = "" );
b := cgi.key_exists( "xyzabc_fake", 1 );
pragma assert( b = false );
n := cgi.key_count( "xyzabc_fake" );
pragma assert( n = 0 );
n := cgi.argument_count;
pragma assert( n = 0 );

-- backquotes

c := `echo x;`;
pragma assert( c = 'x' );
s := `echo x y;`;
pragma assert( s = "x y" );
c := `echo x | sed 's/x/y/g';`;
pragma assert( c = 'y' );


if false then
   return;    -- should not terminate script
end if;

-- elsif should not run if the "if" is performed

declare

i : integer := 0;

function test_f1 return boolean is
begin
  i := i + 1;
  return true;
end test_f1;

function test_f2 return boolean is
begin
  return true;
end test_f2;

begin

if test_f1 then
   null;
elsif test_f1 then
   null;
end if;
pragma assert( i = 1 );

-- i := 0; -- can't do this, side-effects
if not test_f1 then
   null;
elsif not test_f1 then
   null;
end if;
pragma assert( i = 3 );

-- test that @ is ? not fn's return
-- When using debugging messages during development, the following test may
-- have to be commented out.  Restore it afterwards.
-- Restore these DEBUG
s := `put( test_f2 ) @ ( test_f2 );`;
pragma assert( s = "truetrue" );

end;

-- return when clause

declare

function test_f1 return boolean is
begin
  return true when 1=1;
  return false;
end test_f1;

function test_f2 return boolean is
begin
  return true when 1=0;
  return false;
end test_f2;

b : boolean;

procedure test_f3 is
begin
  b := true;
  return when 1=1;
  b := false;
end test_f3;

begin
  b := test_f1;
  pragma assert( b );
  b := test_f2;
  pragma assert( not b );
  test_f3;
  pragma assert( b );
end;

-- Exception handling

declare
  procedure divide( x : integer; y : integer ) is
  begin
    i := x / y;
  exception when others =>
    i := 0;
  end divide;
begin
  divide( 4, 2 );
  pragma assert( i = 2 );
  divide( 4, 0 );
  pragma assert( i = 0 );
end;

declare
  function fdivide( x : integer; y : integer ) return integer is
  begin
    return x / y;
  exception when others =>
    return 0;
  end fdivide;
begin
  i := fdivide( 4, 2 );
  pragma assert( i = 2 );
  i := fdivide( 4, 0 );
  pragma assert( i = 0 );
end;

declare
  procedure divide_for( x : integer; y : integer ) is
  begin
    i := x / y;
    -- for should not execute on error
    for j in 1..2 loop
       i := i + 1;
    end loop;
  exception when others =>
    i := 0;
  end divide_for;
begin
  divide_for( 4, 2 );
  pragma assert( i = 4 );
  divide_for( 4, 0 );
  pragma assert( i = 0 );
end;

declare
  function fdivide_while( x : integer; y : integer ) return integer is
    i : integer;
    j : integer;
  begin
    j := 1;
    i := x/y;
    while j <= 2 loop
       i := i + 1;
       j := j + 1;
    end loop;
    return i;
  exception when others =>
    return 0;
  end fdivide_while;
begin
  i := fdivide_while( 4, 2 );
  pragma assert( i = 4 );
  i := fdivide_while( 4, 0 );
  pragma assert( i = 0 );
end;

declare
  exc1 : exception;
begin
  raise exc1;
  exception when exc1 =>
    null;
end;

declare
  exc2 : exception with "foobar" use 2;
begin
  raise exc2;
  exception when exc2 =>
    null;
end;

declare
  exc3 : exception with "foobar" use 2;
begin
  raise exc3;
  exception when exc3 =>
    pragma assert( exceptions.exception_name = "exc3" );
    pragma assert( exceptions.exception_status_code = 2 );
    pragma assert( strings.index( exceptions.exception_info, "foobar" ) > 0 );
end;

declare
  exc4 : exception with "foobar" use 3;
begin
  raise exc4 with "new message";
  exception when exc4 =>
    pragma assert( exceptions.exception_name = "exc4" );
    pragma assert( exceptions.exception_status_code = 3 );
    pragma assert( strings.index( exceptions.exception_info, "new message" ) > 0 );
end;

-- exc is ok if the outer exc is not an exception

declare
  exc5 : integer;
begin
  declare
    exc5 : exception;
  begin
    null;
  exception when exc5 => null;
  end;
  exc5 := 0;
end;

-- conditional raise

i := 1;
declare
  exc6 : exception;
begin
  raise exc6 when false;
exception when others =>
  i := 2;
end;
pragma assert( i = 1 );

i := 1;
declare
  exc7 : exception;
begin
  raise exc7 when true;
exception when others =>
  i := 2;
end;
pragma assert( i = 2 );

-- inner handler should not run

begin
  i := 1;
  i := 1/0;
  begin
    i := 2;
  exception when others =>
    i := 3;
  end;
  i := 4;
 exception when others =>
    i := 5;
end;
pragma assert( i = 5 );

-- chains

declare
  procedure itself_test is
  begin
    pragma assert( chains.in_chain = false );
    pragma assert( chains.chain_context = chains.not_in_chain );
  end itself_test;
begin
  itself_test;
end;

declare
  procedure itself_test is
  begin
    pragma assert( chains.in_chain = true );
    pragma assert( chains.chain_context /= chains.not_in_chain );
    pragma assert( chains.chain_count >= 1 and chains.chain_count <= 2 );
  end itself_test;
begin
  itself_test @ ;
end;

-- begin block should not obscure chain

declare
  procedure itself_test is
  begin
    begin
      pragma assert( chains.in_chain = true );
      pragma assert( chains.chain_context /= chains.not_in_chain );
      pragma assert( chains.chain_count >= 1 and chains.chain_count <= 2 );
   end;
  end itself_test;
begin
  itself_test @ ;
end;

-- declare block should not obscure chain

declare
  procedure itself_test is
  begin
    declare
      i : integer;
    begin
      i := 0;
      pragma assert( chains.in_chain = true );
      pragma assert( chains.chain_context /= chains.not_in_chain );
      pragma assert( chains.chain_count >= 1 and chains.chain_count <= 2 );
   end;
  end itself_test;
begin
  itself_test @ ;
end;

-- while loop should not obscure chain

declare
  procedure itself_test is
  begin
    while true loop
      pragma assert( chains.in_chain = true );
      pragma assert( chains.chain_context /= chains.not_in_chain );
      pragma assert( chains.chain_count >= 1 and chains.chain_count <= 2 );
   exit;
   end loop;
  end itself_test;
begin
  itself_test @ ;
end;

-- for loop should not obscure chain

declare
  procedure itself_test is
  begin
    for i in 1..1 loop
      pragma assert( chains.in_chain = true );
      pragma assert( chains.chain_context /= chains.not_in_chain );
      pragma assert( chains.chain_count >= 1 and chains.chain_count <= 2 );
   end loop;
  end itself_test;
begin
  itself_test @ ;
end;

-- loop loop should not obscure chain

declare
  procedure itself_test is
  begin
    loop
      pragma assert( chains.in_chain = true );
      pragma assert( chains.chain_context /= chains.not_in_chain );
      pragma assert( chains.chain_count >= 1 and chains.chain_count <= 2 );
   exit;
   end loop;
  end itself_test;
begin
  itself_test @ ;
end;

-- nested procedure should obscure chain

declare
  procedure itself_test is
  begin
    loop
      pragma assert( chains.in_chain = false );
   exit;
   end loop;
  end itself_test;

  procedure itself2_test is
  begin
    itself_test;
  end itself2_test;
begin
  itself2_test @ ;
end;

-- return in inner block

declare
  i : integer := 1;

  procedure return_test is
  begin
    begin
      return;
    end;
    i := 2;
  end return_test;

  function return_test2 return integer is
  begin
    begin
      return 3;
    end;
    return 2;
  end return_test2;

  procedure return_test3 is
  begin
    declare
      j : integer := 4;
    begin
      i := j;
      return;
    end;
    i := 2;
  end return_test3;

  function return_test4 return integer is
  begin
    declare
      j : integer := 5;
    begin
      return j;
    end;
    return 2;
  end return_test4;

begin
  return_test;
  pragma assert( i = 1 );
  i := return_test2;
  pragma assert( i = 3 );
  return_test3;
  pragma assert( i = 4 );
  i := return_test4;
  pragma assert( i = 5 );
end;

-- integer casting
-- in bush, literals are universal_numeric's

i := 2.4;
pragma assert( i = 2 );
i := 2.5;
pragma assert( i = 3 );
i := 2.5 * 2;
pragma assert( i = 5 );
i := 5 / 2;
pragma assert( i = 3 );
i := 2.5 + 1;
pragma assert( i = 4 );
i := 2.5 - 1;
pragma assert( i = 2 );
i := integer( 2.5 ) * 2;
pragma assert( i = 6 );
i := integer( 5.4 ) / 2;
pragma assert( i = 3 );
i := integer( 2.5 ) + 1;
pragma assert( i = 4 );
i := integer( 2.5 ) - 1;
pragma assert( i = 2 );
i := integer( 2.5 ) * 2;
pragma assert( i = 6 );
iz : integer := 2.5;
pragma assert( iz = 3 );
i := short_short_integer( 2.5 ) * 2;
pragma assert( i = 6 );
i := short_integer( 2.5 ) * 2;
pragma assert( i = 6 );
i := long_integer( 2.5 ) * 2;
pragma assert( i = 6 );
i := long_long_integer( 2.5 ) * 2;
pragma assert( i = 6 );

-- integer compatibility with user functions
-- TODO: should also do integer

declare

  function nat_func1 ( nat : natural ) return natural is
  begin
    return nat;
  end nat_func1;

  procedure nat_proc1 ( nat : natural ) is
  begin
    n := nat;
  end nat_proc1;

begin
  p := 98;
  n := nat_func1( p );
  pragma assert( n = 98 );
  p := 97;
  nat_proc1( p );
  pragma assert( n = 97 );
  n := p; -- this should succeed
  i := n;
  i := p;
end;

-- Test env.  I don't check the result, only
-- that it doesn't explode.
env ut;
env un;
env us;
env i;
env s;
env i;
env si;
env ssi;
env li;
env lli;
env f;
env lfv;
env sf;
env dur;
env p;
env n;
env c;
env c1;
env b;
env s;
env s1;
env ft;
--env st;
env fm;
env js;
-- AdaScript style
env( "js" );
env;

-- Other built-ins

clear;
help clear;
pwd;
pwd_cmd : limited command := "/bin/pwd";
pwd_cmd;
jobs;

-- doubly_linked_lists positive tests

-- Basic Assign (GCC Ada 4.7/8+)

declare
  l1 : doubly_linked_lists.list( integer );
  l2 : doubly_linked_lists.list( integer );
begin
  doubly_linked_lists.assign( l1, l2 );
end;

-- Basic Append

declare
  l1 : doubly_linked_lists.list( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
end;

-- Basic Move

declare
  l1 : doubly_linked_lists.list( integer );
  l2 : doubly_linked_lists.list( integer );
begin
  doubly_linked_lists.move( l1, l2 );
end;

-- Basic Is_Empty

declare
  l1 : doubly_linked_lists.list( integer );
begin
  pragma assert( doubly_linked_lists.is_empty( l1 ) );
  doubly_linked_lists.append( l1, 1234 );
  pragma assert( not doubly_linked_lists.is_empty( l1 ) );
end;

-- Clear

declare
  l1 : doubly_linked_lists.list( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.clear( l1 );
  pragma assert( doubly_linked_lists.is_empty( l1 ) );
end;

-- Basic Length

declare
  l1 : doubly_linked_lists.list( integer );
  c : containers.count_type;
begin
  c := doubly_linked_lists.length( l1 );
  pragma assert( c = 0 );
  doubly_linked_lists.append( l1, 1234 );
  c := doubly_linked_lists.length( l1 );
  pragma assert( c = 1 );
end;

-- First_Element

declare
  l1 : doubly_linked_lists.list( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  pragma assert( doubly_linked_lists.first_element( l1 ) = 1234 );
end;

-- Last_Element

declare
  l1 : doubly_linked_lists.list( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  pragma assert( doubly_linked_lists.last_element( l1 ) = 2345 );
end;

-- Delete_First

declare
  l1 : doubly_linked_lists.list( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.delete_first( l1 );
  pragma assert( doubly_linked_lists.first_element( l1 ) = 2345 );
  doubly_linked_lists.append( l1, 3456 );
  doubly_linked_lists.append( l1, 4567 );
  doubly_linked_lists.delete_first( l1, 2 );
  pragma assert( doubly_linked_lists.first_element( l1 ) = 4567 );
end;

-- Delete_Last

declare
  l1 : doubly_linked_lists.list( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.delete_last( l1 );
  pragma assert( doubly_linked_lists.last_element( l1 ) = 1234 );
  doubly_linked_lists.append( l1, 3456 );
  doubly_linked_lists.append( l1, 4567 );
  doubly_linked_lists.delete_last( l1, 2 );
  pragma assert( doubly_linked_lists.last_element( l1 ) = 1234 );
end;

-- First / Element

declare
  l1 : doubly_linked_lists.list( integer );
  c1 : doubly_linked_lists.cursor( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.first( l1, c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 1234 );
end;

-- Last

declare
  l1 : doubly_linked_lists.list( integer );
  c1 : doubly_linked_lists.cursor( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.last( l1, c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 2345 );
end;

-- Next / Previous

declare
  l1 : doubly_linked_lists.list( integer );
  c1 : doubly_linked_lists.cursor( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.last( l1, c1 );
  doubly_linked_lists.previous( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 1234 );
  doubly_linked_lists.first( l1, c1 );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 2345 );
end;

-- Replace_Element

declare
  l1 : doubly_linked_lists.list( integer );
  c1 : doubly_linked_lists.cursor( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.last( l1, c1 );
  doubly_linked_lists.replace_element( l1, c1, 3456 );
  doubly_linked_lists.last( l1, c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 3456 );
end;

-- Delete / Multi-append

declare
  l1 : doubly_linked_lists.list( integer );
  c1 : doubly_linked_lists.cursor( integer );
begin
  doubly_linked_lists.append( l1, 1234, 4 );
  pragma assert( doubly_linked_lists.length( l1 ) = 4 );
  doubly_linked_lists.first( l1, c1 );
  doubly_linked_lists.delete( l1, c1 );
  pragma assert( doubly_linked_lists.length( l1 ) = 3 );
  doubly_linked_lists.prepend( l1, 1234, 3 );
  doubly_linked_lists.first( l1, c1 );
  doubly_linked_lists.delete( l1, c1, 2 );
  pragma assert( doubly_linked_lists.length( l1 ) = 4 );
end;

-- Contains / Find / Hash_Element

declare
  l1 : doubly_linked_lists.list( integer );
  c1 : doubly_linked_lists.cursor( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.append( l1, 3456 );
  pragma assert( doubly_linked_lists.contains( l1, 1234 ) );
  pragma assert( not doubly_linked_lists.contains( l1, 7890 ) );
  doubly_linked_lists.find( l1, 2345, c1 );
  pragma assert( doubly_linked_lists.has_element( c1 ) );
  doubly_linked_lists.find( l1, 7890, c1 );
  pragma assert( not doubly_linked_lists.has_element( c1 ) );
  doubly_linked_lists.reverse_find( l1, 2345, c1 );
  pragma assert( doubly_linked_lists.has_element( c1 ) );
  doubly_linked_lists.reverse_find( l1, 7890, c1 );
  pragma assert( not doubly_linked_lists.has_element( c1 ) );
end;

-- Reverse_Elements / Flip

declare
  l1 : doubly_linked_lists.list( integer );
  c1 : doubly_linked_lists.cursor( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.append( l1, 3456 );
  doubly_linked_lists.reverse_elements( l1 );
  doubly_linked_lists.first( l1, c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 3456 );
  doubly_linked_lists.flip( l1 );
  doubly_linked_lists.first( l1, c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 1234 );
end;

-- Swap

declare
  l1 : doubly_linked_lists.list( integer );
  c1 : doubly_linked_lists.cursor( integer );
  c2 : doubly_linked_lists.cursor( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.append( l1, 3456 );
  doubly_linked_lists.first( l1, c1 );
  doubly_linked_lists.last( l1, c2 );
  doubly_linked_lists.swap( l1, c1, c2 );
  pragma assert( doubly_linked_lists.element( c1 ) = 3456 );
  pragma assert( doubly_linked_lists.element( c2 ) = 1234 );
  doubly_linked_lists.swap_links( l1, c1, c2 );
  doubly_linked_lists.first( l1, c1 );
  doubly_linked_lists.last( l1, c2 );
  pragma assert( doubly_linked_lists.element( c1 ) = 1234 );
  pragma assert( doubly_linked_lists.element( c2 ) = 3456 );
end;

-- Insert_Before

declare
  l1 : doubly_linked_lists.list( integer );
  c1 : doubly_linked_lists.cursor( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.append( l1, 3456 );

  -- Variation 1

  doubly_linked_lists.first( l1, c1 );
  doubly_linked_lists.insert_before( l1, c1, 4567 );
  doubly_linked_lists.first( l1, c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 4567 );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 1234 );

  -- Variation 2

  doubly_linked_lists.first( l1, c1 );
  doubly_linked_lists.insert_before( l1, c1, 5678, 1+1 );
  doubly_linked_lists.first( l1, c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 5678 );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 5678 );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 4567 );
end;

-- Insert_Before_And_Mark (Version 3)

declare
  l1 : doubly_linked_lists.list( integer );
  c1 : doubly_linked_lists.cursor( integer );
  c2 : doubly_linked_lists.cursor( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.append( l1, 3456 );

  doubly_linked_lists.first( l1, c1 );
  doubly_linked_lists.insert_before_and_mark( l1, c1, 4567, c2 );

  pragma assert( doubly_linked_lists.length( l1 ) = 4 );
  pragma assert( doubly_linked_lists.element( c1 ) = 1234 );
  pragma assert( doubly_linked_lists.element( c2 ) = 4567 );
  doubly_linked_lists.next( c2 );
  pragma assert( doubly_linked_lists.element( c2 ) = 1234 );
end;

-- Insert_Before_And_Mark (Version 4)

declare
  l1 : doubly_linked_lists.list( integer );
  c1 : doubly_linked_lists.cursor( integer );
  c2 : doubly_linked_lists.cursor( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.append( l1, 3456 );

  doubly_linked_lists.first( l1, c1 );
  doubly_linked_lists.insert_before_and_mark( l1, c1, 4567, c2, 2 );

  pragma assert( doubly_linked_lists.length( l1 ) = 5 );
  pragma assert( doubly_linked_lists.element( c1 ) = 1234 );
  pragma assert( doubly_linked_lists.element( c2 ) = 4567 );
  doubly_linked_lists.next( c2 );
  pragma assert( doubly_linked_lists.element( c2 ) = 4567 );
  doubly_linked_lists.next( c2 );
  pragma assert( doubly_linked_lists.element( c2 ) = 1234 );
end;

-- Insert_Before_And_Mark (Version 5)

declare
  l1 : doubly_linked_lists.list( integer );
  c1 : doubly_linked_lists.cursor( integer );
  c2 : doubly_linked_lists.cursor( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.append( l1, 3456 );

  doubly_linked_lists.first( l1, c1 );
  doubly_linked_lists.insert_before_and_mark( l1, c1, c2 );

  pragma assert( doubly_linked_lists.length( l1 ) = 4 );
  doubly_linked_lists.replace_element( l1, c2, 4567 );
  pragma assert( doubly_linked_lists.element( c1 ) = 1234 );
  doubly_linked_lists.first( l1, c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 4567 );
end;

-- Insert_Before_And_Mark (Version 6)

declare
  l1 : doubly_linked_lists.list( integer );
  c1 : doubly_linked_lists.cursor( integer );
  c2 : doubly_linked_lists.cursor( integer );
begin
  doubly_linked_lists.append( l1, 1234 );
  doubly_linked_lists.append( l1, 2345 );
  doubly_linked_lists.append( l1, 3456 );

  doubly_linked_lists.first( l1, c1 );
  doubly_linked_lists.insert_before_and_mark( l1, c1, c2, 1+1 );

  pragma assert( doubly_linked_lists.length( l1 ) = 5 );
  doubly_linked_lists.replace_element( l1, c2, 4567 );
  doubly_linked_lists.next( c2 );
  doubly_linked_lists.replace_element( l1, c2, 5678 );
  pragma assert( doubly_linked_lists.element( c1 ) = 1234 );
  doubly_linked_lists.first( l1, c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 4567 );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = 5678 );
end;

-- Splice Variation 1: copying one list to another

declare
  l1 : doubly_linked_lists.list( string );
  l2 : doubly_linked_lists.list( string );
  c1 : doubly_linked_lists.cursor( string );
begin
  doubly_linked_lists.append( l1, "apple" ) @ ( l1, "banana" ) @ ( l1, "cherry" );
  doubly_linked_lists.append( l2, "donut" ) @ ( l2, "eclair" ) @ (l2, "gelato" );

  doubly_linked_lists.first( l1, c1 );
  doubly_linked_lists.next( c1 );

  doubly_linked_lists.splice( l1, c1, l2 );  -- add list 2 before "banana"
  pragma assert( doubly_linked_lists.length( l1 ) = 6 );

  doubly_linked_lists.first( l1, c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = "apple" );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = "donut" );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = "eclair" );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = "gelato" );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = "banana" );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = "cherry" );
end;

-- Splice Variation 2: copying one element from one list to another

declare
  l1 : doubly_linked_lists.list( string );
  l2 : doubly_linked_lists.list( string );
  c1 : doubly_linked_lists.cursor( string );
  c2 : doubly_linked_lists.cursor( string );
begin
  doubly_linked_lists.append( l1, "apple" ) @ ( l1, "banana" ) @ ( l1, "cherry" );
  doubly_linked_lists.append( l2, "donut" ) @ ( l2, "eclair" ) @ (l2, "gelato" );

  doubly_linked_lists.last( l1, c1 );
  doubly_linked_lists.first( l2, c2 );
  doubly_linked_lists.next( c2 );

  doubly_linked_lists.splice( l1, c1, l2, c2 );  -- add "eclair" only to list l1 before "cherry"
  pragma assert( doubly_linked_lists.length( l1 ) = 4 );

  doubly_linked_lists.first( l1, c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = "apple" );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = "banana" );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = "eclair" );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = "cherry" );
end;

-- Splice Variation 3: a single list

declare
  l1 : doubly_linked_lists.list( string );
  c1 : doubly_linked_lists.cursor( string );
  c2 : doubly_linked_lists.cursor( string );
begin
  doubly_linked_lists.append( l1, "apple" ) @ ( l1, "banana" ) @ ( l1, "cherry" );

  doubly_linked_lists.first( l1, c1 );
  doubly_linked_lists.last( l1, c2 );

  doubly_linked_lists.splice( l1, c1, c2 );  -- move "cherry" before "apple"
  pragma assert( doubly_linked_lists.length( l1 ) = 3 );

  doubly_linked_lists.first( l1, c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = "cherry" );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = "apple" );
  doubly_linked_lists.next( c1 );
  pragma assert( doubly_linked_lists.element( c1 ) = "banana" );
end;

-- dynamic hash tables

declare
  t   : dynamic_hash_tables.table( string );
  t2  : dynamic_hash_tables.table( integer );
  s   : string;
  eof : boolean;
begin

  -- dynamic hash gets /sets

  dynamic_hash_tables.set( t, "foo", "bar" );
  dynamic_hash_tables.reset( t );
  dynamic_hash_tables.set( t, "foo", "bar" );
  pragma assert( dynamic_hash_tables.has_element( t, "foo" ) );
  pragma assert( dynamic_hash_tables.get( t, "foo" ) = "bar" );
  dynamic_hash_tables.reset( t );
  pragma assert( not dynamic_hash_tables.has_element( t, "foo" ) );
  pragma assert( dynamic_hash_tables.get( t, "foo" ) = "" );
  dynamic_hash_tables.set( t, "foo", "bar" );
  dynamic_hash_tables.remove( t, "foo" );
  pragma assert( dynamic_hash_tables.get( t, "foo" ) = "" );
  dynamic_hash_tables.set( t, "foo", "bar" );
  dynamic_hash_tables.add( t, "foo", "bar2" );
  pragma assert( dynamic_hash_tables.get( t, "foo" ) = "bar" );
  dynamic_hash_tables.add( t, "foo2", "bar2" );
  pragma assert( dynamic_hash_tables.get( t, "foo2" ) = "bar2" );
  dynamic_hash_tables.replace( t, "foo2", "bar3" );
  pragma assert( dynamic_hash_tables.get( t, "foo2" ) = "bar3" );
  dynamic_hash_tables.replace( t, "foo3", "bar3" );
  pragma assert( dynamic_hash_tables.get( t, "foo3" ) = "" );
  dynamic_hash_tables.prepend( t, "foo", "start" );
  dynamic_hash_tables.append( t, "foo", "end" );
  pragma assert( dynamic_hash_tables.get( t, "foo" ) = "startbarend" );
  dynamic_hash_tables.prepend( t, "foo4", "start" );
  dynamic_hash_tables.append( t, "foo4", "end" );
  pragma assert( dynamic_hash_tables.get( t, "foo4" ) = "" );

  -- dynamic hash increment / decrement

  dynamic_hash_tables.set( t2, "counter", 0 );
  dynamic_hash_tables.increment( t2, "counter" );
  pragma assert( dynamic_hash_tables.get( t2, "counter" ) = 1 );
  dynamic_hash_tables.increment( t2, "counter", 2 );
  pragma assert( dynamic_hash_tables.get( t2, "counter" ) = 3 );
  dynamic_hash_tables.increment( t2, "counter2" );
  pragma assert( not dynamic_hash_tables.has_element( t2, "counter2" ) );

  dynamic_hash_tables.set( t2, "counter", 0 );
  dynamic_hash_tables.decrement( t2, "counter" );
  pragma assert( dynamic_hash_tables.get( t2, "counter" ) = -1 );
  dynamic_hash_tables.decrement( t2, "counter", 2 );
  pragma assert( dynamic_hash_tables.get( t2, "counter" ) = -3 );
  dynamic_hash_tables.decrement( t2, "counter2" );
  pragma assert( not dynamic_hash_tables.has_element( t2, "counter2" ) );

  -- dynamic hash traversal

  dynamic_hash_tables.reset( t );
  dynamic_hash_tables.set( t, "foo", "bar" );
  dynamic_hash_tables.get_first( t, s, eof );
  pragma assert( s  = "bar" );
  pragma assert( not eof );
  dynamic_hash_tables.get_next( t, s, eof );
  pragma assert( eof );

end;

-- Assemble

declare
  s : string;
  sl : doubly_linked_lists.list( string );
begin
  doubly_linked_lists.clear( sl );
  doubly_linked_lists.append( sl, "ABC" );
  doubly_linked_lists.append( sl, "DEF" );
  doubly_linked_lists.append( sl, "GHI" );
  s := doubly_linked_lists.assemble( sl );

  -- This should be os independent
  pragma assert( s = "ABC" & ASCII.LF & "DEF" & ASCII.LF & "GHI" );

  s := doubly_linked_lists.assemble( sl, "ab" );
  pragma assert( s = "ABCabDEFabGHI" );

  s := doubly_linked_lists.assemble( sl, "ab", "cd" );
  pragma assert( s = "ABCabDEFabGHIcd" );

  doubly_linked_lists.clear( sl );
  s := doubly_linked_lists.assemble( sl );
  pragma assert( s = "" );

  doubly_linked_lists.clear( sl );
  s := doubly_linked_lists.assemble( sl, ASCII.LF, "foo" );
  pragma assert( s = "foo" );
end;

-- Disassemble

declare
  s : string;
  sl : doubly_linked_lists.list( string );
  sc : doubly_linked_lists.cursor( string );
begin
  s := "ABC" & ASCII.LF & "DEF" & ASCII.LF & "GHI";
  doubly_linked_lists.disassemble( s, sl, ASCII.LF, "GHI" );
  doubly_linked_lists.first( sl, sc );
  pragma assert( strings.to_escaped( doubly_linked_lists.element( sc ) ) = "ABC" );
  doubly_linked_lists.next( sc );
  pragma assert( strings.to_escaped( doubly_linked_lists.element( sc ) ) = "DEF" );
  doubly_linked_lists.next( sc );
  pragma assert( not doubly_linked_lists.has_element( sc ) );

  doubly_linked_lists.clear( sl );
  s := "ABC";
  doubly_linked_lists.disassemble( s, sl );
  doubly_linked_lists.first( sl, sc );
  pragma assert( strings.to_escaped( doubly_linked_lists.element( sc ) ) = "ABC" );
  doubly_linked_lists.next( sc );
  pragma assert( not doubly_linked_lists.has_element( sc ) );

  doubly_linked_lists.clear( sl );
  s := "ABC";
  doubly_linked_lists.disassemble( s, sl, ASCII.LF, "XYZ" );
  doubly_linked_lists.first( sl, sc );
  pragma assert( strings.to_escaped( doubly_linked_lists.element( sc ) ) = "ABC" );
  doubly_linked_lists.next( sc );
  pragma assert( not doubly_linked_lists.has_element( sc ) );

  doubly_linked_lists.clear( sl );
  s := "ABCabDEFabGHIab";
  doubly_linked_lists.disassemble( s, sl, "ab" );
  doubly_linked_lists.first( sl, sc );
  pragma assert( strings.to_escaped( doubly_linked_lists.element( sc ) ) = "ABC" );
  doubly_linked_lists.next( sc );
  pragma assert( strings.to_escaped( doubly_linked_lists.element( sc ) ) = "DEF" );
  doubly_linked_lists.next( sc );
  pragma assert( strings.to_escaped( doubly_linked_lists.element( sc ) ) = "GHI" );
  doubly_linked_lists.next( sc );
  pragma assert( not doubly_linked_lists.has_element( sc ) );

  doubly_linked_lists.clear( sl );
  s := "";
  doubly_linked_lists.disassemble( s, sl );
  doubly_linked_lists.first( sl, sc );
  pragma assert( not doubly_linked_lists.has_element( sc ) );
end;

-- renaming tests

begin

-- integers

declare
  i : integer := 5;
  j : integer renames i;
begin
  pragma assert( i = j );
  j := 6;
  pragma assert( i = j );
end;

-- two renamings

declare
  i : integer := 5;
  j : integer renames i;
  k : integer renames i;
begin
  k := 6;
  pragma assert( i = k );
  pragma assert( j = k );
end;

-- renaming of a renaming

declare
  i : integer := 5;
  j : integer renames i;
  m : integer renames j;
begin
  m := 7;
  pragma assert( i = m );
  pragma assert( j = m );
end;

-- constant

declare
  b : constant integer := 1;
  b2 : constant integer renames b;
begin
  null;
  pragma assert( b2 = b );
end;

-- system constant

declare
  qu : constant character renames Latin_1.Quotation;
begin
  null;
  pragma assert( qu = '"' );
end;

-- read-only renaming

declare
  i : integer := 5;
  c : constant integer renames i;
begin
  null;
  pragma assert( c = i );
end;

-- subtypes and renamings

declare
  subtype myint is integer;
  i2 : integer := 9;
  j2 : myint renames i2;
begin
  null;
  pragma assert( j2 = 9 );
end;

-- record fields

declare
  i : integer;
  type rt is record
    f : integer;
  end record;
  r : rt;
  ra : integer renames r.f;
  r2 : rt renames r;
  r3 : rt renames r2;
  subtype rt2 is rt;
  r4 : rt2 renames r;
begin
  r.f := 8;
  i := r.f;
  pragma assert( ra = 8 );
  pragma assert( r2.f = 8 );
  pragma assert( r3.f = 8 );
  pragma assert( r4.f = 8 );
end;

-- arrays

declare
  type a_type is array( 1..2 ) of integer;
  a : a_type;
  ar2 : a_type renames a;
  ar3 : a_type renames ar2;
  subtype a2_type is a_type;
  ar4 : a2_type renames a;
  ae : integer renames a(1);
begin
  a(1) := 2;
  a(2) := 3;
  pragma assert( ar2(2) = 3 );
  pragma assert( ar3(2) = 3 );
  pragma assert( ar4(2) = 3 );
  pragma assert( ae = 2 );
end;

-- commands

declare
  x : limited command := "/bin/true";
  x2 : limited command renames x;
begin
  x;
  x2;
end;

-- enumerated variables

declare
  type fruit is (apple, banana, cherry );
  f : fruit;
  g : fruit renames f;
begin
  g := apple;
  pragma assert( f = g );
end;

end; -- renaming tests

-- copying tests

begin

-- integers

declare
  i : integer := 5;
  j : integer copies i;
begin
  pragma assert( i = j );
  j := 6;
  pragma assert( i = 5 );
  pragma assert( j = 6 );
end;

-- two copies

declare
  i : integer := 5;
  j : integer copies i;
  k : integer copies i;
begin
  pragma assert( j = i );
  pragma assert( j = k );
  j := 6;
  k := 7;
  pragma assert( i = 5 );
  pragma assert( j = 6 );
  pragma assert( k = 7 );
end;

-- copy of a copy

declare
  i : integer := 5;
  j : integer copies i;
  m : integer copies j;
begin
  pragma assert( i = j );
  pragma assert( i = m );
  m := 7;
  pragma assert( i = 5 );
  pragma assert( i = 5 );
  pragma assert( m = 7 );
  j := 6;
  pragma assert( i = 5 );
  pragma assert( j = 6 );
  pragma assert( m = 7 );
end;

-- constant

declare
  b : constant integer := 1;
  b2 : constant integer copies b;
begin
  null;
  pragma assert( b = 1 );
  pragma assert( b2 = b );
end;

-- system constant

declare
  qu : constant character copies Latin_1.Quotation;
begin
  null;
  pragma assert( qu = '"' );
end;

-- read-only copying

declare
  i : integer := 5;
  c : constant integer copies i;
begin
  pragma assert( c = 5 );
  pragma assert( c = i );
  i := 4;
  pragma assert( i = 4 );
  pragma assert( c = 5 );
end;

-- subtypes and copies

declare
  subtype myint is integer;
  i2 : integer := 9;
  j2 : myint copies i2;
begin
  null;
  pragma assert( j2 = 9 );
end;

-- enumerated variables

declare
  type fruit is (apple, banana, cherry );
  f : fruit;
begin
  f := apple;
  declare
    g : fruit copies f;
  begin
    null;
    pragma assert( f = g );
  end;
end;

-- Copy a checked volatile

declare
  c1 : constant integer := 5;
  pragma volatile( c1 );

  c2 : constant integer copies c1;
begin
  null;
  pragma assert( c2 = 5 );
end;

-- Copy an array element

declare
  a1 : array(1..1) of integer := (15);
  c1 : constant integer copies a1(1);
begin
  null;
  pragma assert( c1 = 15 );
end;

end; --copy tests


-- in out parameter tests

begin

-- basic in out procedure

declare
  procedure in_out_proc ( i : in out integer ) is
  begin
    i := i * 2;
  end in_out_proc;
  j : integer := 5;
begin
  in_out_proc( j );
  pragma assert( j = 10 );
end;

-- basic in out procedure - string

declare
  procedure in_out_proc ( s : in out string ) is
  begin
    s := s & "bar";
  end in_out_proc;
  j : string := "foo";
begin
  in_out_proc( j );
  pragma assert( j = "foobar" );
end;

-- basic in out procedure - enumerated (boolean)

declare
  procedure in_out_proc ( b : in out boolean ) is
  begin
    b := not b;
  end in_out_proc;
  j : boolean := true;
begin
  in_out_proc( j );
  pragma assert( j = false );
end;

-- basic in out procedure - subtype

declare
  subtype myint is integer;
  procedure in_out_proc ( i : in out myint ) is
  begin
    i := i * 2;
  end in_out_proc;
  j : integer := 3;
begin
  in_out_proc( j );
  pragma assert( j = 6 );
end;

-- in out array element

declare
  procedure in_out_proc ( i : in out integer ) is
  begin
    i := i * 2;
  end in_out_proc;
  type param_array is array(1..1) of integer;
  p : param_array;
begin
  p(1) := 6;
  in_out_proc( p(1) );
  pragma assert( p(1) = 12 );
end;

-- in out full array

declare
  type param_array is array(1..1) of integer;
  procedure in_out_proc ( a : in out param_array ) is
  begin
    a(1) := a(1) * 2;
  end in_out_proc;
  p : param_array;
begin
  p(1) := 7;
  in_out_proc( p );
  pragma assert( p(1) = 14 );
end;

-- in out record field

declare
  procedure in_out_proc ( i : in out integer ) is
  begin
    i := i * 2;
  end in_out_proc;
  type rt is record
    j : integer;
  end record;
  r : rt;
begin
  r.j := 8;
  in_out_proc( r.j );
  pragma assert( r.j = 16 );
end;

-- in out full record

declare
  type rt is record
    j : integer;
  end record;
  procedure in_out_proc ( p : in out rt ) is
  begin
    p.j := p.j * 2;
  end in_out_proc;
  r : rt;
begin
  r.j := 9;
  in_out_proc( r );
  pragma assert( r.j = 18 );
end;

-- nested in out parameters

declare
  procedure in_out_proc1 ( i : in out integer ) is
  begin
    i := i * 2;
  end in_out_proc1;
  procedure in_out_proc2 ( i : in out integer ) is
  begin
    in_out_proc1( i );
    i := i * 3;
  end in_out_proc2;
  j : integer := 4;
begin
  in_out_proc2( j );
  pragma assert( j = 24 );
end;

-- in out function

declare
  function in_out_func ( i : in out integer ) return integer is
  begin
    i := i * 2;
    return i * 2;
  end in_out_func;
  j : integer := 1;
  v : integer;
begin
  v := in_out_func( j );
  pragma assert( j = 2 );
  pragma assert( v = 4 );
end;

end;

-- out parameter tests

begin

-- basic out procedure

declare
  procedure out_proc ( i : in out integer ) is
  begin
    i := 1;
  end out_proc;
  j : integer;
begin
  out_proc( j );
  pragma assert( j = 1 );
end;

-- basic out procedure - string

declare
  procedure out_proc ( s : in out string ) is
  begin
    s := "bar";
  end out_proc;
  j : string := "foo";
begin
  out_proc( j );
  pragma assert( j = "bar" );
end;

-- basic out procedure - enumerated (boolean)

declare
  procedure out_proc ( b : out boolean ) is
  begin
    b := true;
  end out_proc;
  j : boolean := true;
begin
  out_proc( j );
  pragma assert( j = true );
end;

-- basic out procedure - subtype

declare
  subtype myint is integer;
  procedure out_proc ( i : out myint ) is
  begin
    i := 2;
  end out_proc;
  j : integer;
begin
  out_proc( j );
  pragma assert( j = 2 );
end;

-- out array element

declare
  procedure out_proc ( i : out integer ) is
  begin
    i := 3;
  end out_proc;
  type param_array is array(1..1) of integer;
  p : param_array;
begin
  out_proc( p(1) );
  pragma assert( p(1) = 3 );
end;

-- out full array

declare
  type param_array is array(1..1) of integer;
  procedure out_proc ( a : out param_array ) is
  begin
    a(1) := 4;
  end out_proc;
  p : param_array;
begin
  out_proc( p );
  pragma assert( p(1) = 4 );
end;

-- out record field

declare
  procedure out_proc ( i : out integer ) is
  begin
    i := 5;
  end out_proc;
  type rt is record
    j : integer;
  end record;
  r : rt;
begin
  out_proc( r.j );
  pragma assert( r.j = 5 );
end;

-- out full record

declare
  type rt is record
    j : integer;
  end record;
  procedure out_proc ( p : out rt ) is
  begin
    p.j := 6;
  end out_proc;
  r : rt;
begin
  out_proc( r );
  pragma assert( r.j = 6 );
end;

-- nested out parameters

declare
  procedure out_proc1 ( i : out integer ) is
  begin
    i := 7;
  end out_proc1;
  procedure out_proc2 ( i : out integer ) is
  begin
    out_proc1( i );
  end out_proc2;
  j : integer;
begin
  out_proc2( j );
  pragma assert( j = 7 );
end;

end;

-- user-define subprograms during an exit...ensure syntax
-- checked correctly

declare
  function user_f( b : boolean ) return boolean is
  begin
    return b;
  end user_f;
  procedure user_p( b : boolean ) is
  begin
    ? b;
  end user_p;

  b : boolean := false;
begin
  loop
    exit when true;
    b := user_f( true );
    user_p( true );
  end loop;
  pragma assert( b = false );
end;

-- Other packages: Lock_Files

rm -f "_test.lck" ; -- delete if exists
lock_files.lock_file( "_test.lck" );
lock_files.unlock_file( "_test.lck" );
lock_files.lock_file( "cdtest", "_test.lck" );
lock_files.unlock_file( "cdtest", "_test.lck" );
lock_files.lock_file( "_test.lck", 5 );
lock_files.unlock_file( "_test.lck" );
lock_files.lock_file( "_test.lck", 5, 1 );
lock_files.unlock_file( "_test.lck" );
lock_files.lock_file( "cdtest", "_test.lck", 5 );
lock_files.unlock_file( "cdtest", "_test.lck" );
lock_files.lock_file( "cdtest", "_test.lck", 5, 1 );
lock_files.unlock_file( "cdtest", "_test.lck" );

-- Other packages: dirops

c := directory_operations.dir_separator;
pragma assert( c = "/" or c = "\" or c = ":" );

s := directory_operations.get_current_dir;
pragma assert( s = PWD & "/" );
s := @ & "foobar.txt";
s1 := directory_operations.base_name( s );
pragma assert( s1 = "foobar.txt" );
s1 := directory_operations.base_name( s, ".txt" );
pragma assert( s1 = "foobar" );
s1 := directory_operations.file_name( s );
pragma assert( s1 = "foobar.txt" );
s1 := directory_operations.dir_name( s );
pragma assert( s1 = PWD & "/" );
s1 := directory_operations.file_extension( s );
pragma assert( s1 = ".txt" );
s1 := directory_operations.expand_path( "$PWD/foobar.txt" );
pragma assert( s1 = s );
s1 := directory_operations.expand_path( "$PWD/foobar.txt", environment_style.unix );
pragma assert( s1 /= "" ); -- can't be sure of O/S
s1 := directory_operations.format_pathname( s );
pragma assert( s1 /= "" ); -- can't be sure of O/S
s1 := directory_operations.format_pathname( s, path_style.unix );
pragma assert( s1 /= "" ); -- can't be sure of O/S

directory_operations.change_dir( "cdtest" );
directory_operations.make_dir( "foo" );
declare
  d : directory_operations.dir_type_id;
begin
  pragma assert( not directory_operations.is_open( d ) );
  directory_operations.open( d, "foo" );
  pragma assert( directory_operations.is_open( d ) );
  s := "x";
  while strings.length( s ) > 0 loop
    directory_operations.read( d, s );
    pragma assert( s = "." or s = ".." or s = "" );
  end loop;
  directory_operations.close( d );
  pragma assert( not directory_operations.is_open( d ) );
end;
directory_operations.remove_dir( "foo" );
b := files.exists( "foo" );
pragma assert( not b );
directory_operations.make_dir( "foo" );
directory_operations.make_dir( "foo/bar" );
directory_operations.remove_dir( "foo", true );
b := files.exists( "foo" );
pragma assert( not b );
cd ..;

-- Other packages: gnat.crc32

crc : gnat.crc32.crc32;
gnat.crc32.initialize( crc );
gnat.crc32.update( crc, "abc" );
i := gnat.crc32.get_value( crc );
pragma assert( i = 3403398717 );

-- Other packages: units

lfv := units.inches2mm( 10 );
pragma assert( lfv = 254 );
lfv := numerics.rounding( units.feet2cm( 100 ) );
pragma assert( lfv = 3048 );
lfv := numerics.rounding( units.yards2m( 100 ) );
pragma assert( lfv = 91 );
lfv := numerics.rounding( units.miles2km( 100 ) );
pragma assert( lfv = 161 );
lfv := numerics.rounding( units.mm2inches( 100 ) );
pragma assert( lfv = 4 );
lfv := numerics.rounding( units.cm2inches( 100 ) );
pragma assert( lfv = 39 );
lfv := numerics.rounding( units.m2yards( 100 ) );
pragma assert( lfv = 109 );
lfv := numerics.rounding( units.km2miles( 100 ) ) ;
pragma assert( lfv = 62 );
lfv := numerics.rounding( units.sqin2sqcm( 100 ) );
pragma assert( lfv = 645 );
lfv := numerics.rounding( units.sqft2sqm( 100 ) );
pragma assert( lfv = 9 );
lfv := numerics.rounding( units.sqyd2sqm( 100 ) );
pragma assert( lfv = 84 );
lfv := numerics.rounding( units.acres2hectares( 100 ) );
pragma assert( lfv = 40 );
lfv := numerics.rounding( units.sqcm2sqin( 100 ) ) ;
pragma assert( lfv = 16 );
lfv := numerics.rounding( units.sqm2sqft( 100 ) ) ;
pragma assert( lfv = 1076 );
lfv := numerics.rounding( units.sqm2sqyd( 100 ) );
pragma assert( lfv = 120 );
lfv := numerics.rounding( units.sqkm2sqmiles( 100 ) );
pragma assert( lfv = 39 );
lfv := numerics.rounding( units.hectares2acres( 100 ) );
pragma assert( lfv = 247 );
lfv := numerics.rounding( units.ly2pc( 100 ) );
pragma assert( lfv = 31 );
lfv := numerics.rounding( units.pc2ly( 100 ) );
pragma assert( lfv = 326 );
lfv := numerics.rounding( units.oz2grams( 100 ) );
pragma assert( lfv = 2835 );
lfv := numerics.rounding( units.lb2kg( 100 ) );
pragma assert( lfv = 45 );
lfv := numerics.rounding( units.tons2tonnes( 100 ) );
pragma assert( lfv = 102 );
lfv := numerics.rounding( units.grams2oz( 100 ) );
pragma assert( lfv = 35 );
lfv := numerics.rounding( units.kg2lb( 100 ) );
pragma assert( lfv = 220 );
lfv := numerics.rounding( units.tonnes2tons( 100 ) );
pragma assert( lfv = 98 );
lfv := numerics.rounding( units.floz2ml( 100 ) );
pragma assert( lfv = 2841 );
lfv := numerics.rounding( units.usfloz2ml( 100 ) );
pragma assert( lfv = 2957 );
lfv := numerics.rounding( units.usfloz2floz( 100 ) );
pragma assert( lfv = 104 );
lfv := numerics.rounding( units.pints2l( 100 ) );
pragma assert( lfv = 57 );
lfv := numerics.rounding( units.gal2l( 100 ) );
pragma assert( lfv = 455 );
lfv := numerics.rounding( units.ml2floz( 100 ) ) ;
pragma assert( lfv = 4 );
lfv := numerics.rounding( units.ml2usfloz( 100 ) );
pragma assert( lfv = 3 );
lfv := numerics.rounding( units.floz2usfloz( 100 ) );
pragma assert( lfv = 96 );
lfv := numerics.rounding( units.l2quarts( 100 ) ) ;
pragma assert( lfv = 88 );
lfv := numerics.rounding( units.l2gal( 100 ) );
pragma assert( lfv = 22 );
lfv := numerics.rounding( units.f2c( 100 ) );
pragma assert( lfv = 38 );
lfv := numerics.rounding( units.c2f( 100 ) )  ;
pragma assert( lfv = 212 );
lfv := numerics.rounding( units.k2c( 100 ) );
pragma assert( lfv = -173 );
lfv := numerics.rounding( units.c2k( 100 ) );
pragma assert( lfv = 373 );
lfv := numerics.rounding( units.usdrygal2l( 100 ) );
pragma assert( lfv = 440 );
lfv := numerics.rounding( units.usliqgal2l( 100 ) );
pragma assert( lfv = 379 );
lfv := numerics.rounding( units.l2usliqgal( 100 ) );
pragma assert( lfv = 26 );
lfv := numerics.rounding( units.troz2g( 100 ) );
pragma assert( lfv = 3110 );
lfv := numerics.rounding( units.g2troz( 100 ) );
pragma assert( lfv = 3 );
lfv := numerics.rounding( units.cucm2floz( 100 ) );
pragma assert( lfv = 4 );
lfv := numerics.rounding( units.cucm2usfloz( 100 ) );
pragma assert( lfv = 34 );
lfv := numerics.rounding( units.usfloz2cucm( 100 ) );
pragma assert( lfv = 2957 );
lfv := numerics.rounding( units.bytes2mb( 10000000 ) );
pragma assert( lfv = 10 );
lfv := numerics.rounding( units.mb2bytes( 100 ) );
pragma assert( lfv = 104857600 );

-- source_info

s := source_info.file;
pragma assert( s = "goodtest.sp" );
p := source_info.line;
pragma assert( p > 0 );
s := source_info.source_location;
pragma assert( strings.length( s ) > 0 );
s := source_info.enclosing_entity;
pragma assert( s = "script" );

-- abstract

declare
type abstract_type is new abstract integer;
subtype abstract_subtype is abstract integer;

procedure abstract1 is null abstract;

procedure abstract2 is abstract
begin
  null;
end abstract2;

--procedure abstract3( a1 : abstract_type ) is null abstract;

procedure abstract4( a2 : abstract_type ) is abstract
begin
  ? a2; -- reference abstract parameter
end abstract4;

function abstract5 return boolean is abstract
begin
  return false;
end abstract5;

--function abstract6( a3 : abstract_type ) return boolean is null abstract;

function abstract7( a4 : abstract_type ) return boolean is abstract
begin
  ? a4; -- reference abstract parameter
  return false;
end abstract7;

function abstract8 return abstract_type is null abstract;

begin
  null;
end;

-- limited arrays and records

declare
  la1 : limited array( 1..3 ) of integer := (1,2,3);
  type la2 is limited array( 1..3 ) of integer;
  la3 : la2 := (1,2,3);
  type lr is limited record
     i : integer;
  end record;
  -- no easy way to create a limited record and use it as we currently
  -- need a built-in procedure that accepts one as a paramter.
  pragma assumption( applied, lr );
  pragma assumption( used, la1 );
  pragma assumption( used, la3 );
begin
  null;
end;

-- Constant types

declare
   type ct is new constant integer;
   i : integer;
   c : ct := 1;
begin
   i := integer(c);
end;

-- Forward Declarations

declare

  procedure proc0;

  procedure proc0 is
  begin
    put_line( "zero" );
  end proc0;

  procedure proc1( param1 : integer );

  procedure proc1( param1 : integer ) is
  begin
    put_line( param1 );
  end proc1;

  procedure proc2( param21 : integer; param22 : integer );

  procedure proc2( param21 : integer; param22 : integer ) is
  begin
    put_line( param21 );
    put_line( param22 );
  end proc2;

  function forward_func0 return string;

  function forward_func0 return string is
  begin
    return "ff0";
  end forward_func0;

  function forward_func1( param1 : integer ) return integer;

  function forward_func1( param1 : integer ) return integer is
  begin
    return param1;
  end forward_func1;

  function forward_func2( param1 : integer; param2 : integer ) return integer;

  function forward_func2( param1 : integer; param2 : integer ) return integer is
  begin
    return param1 + param2;
  end forward_func2;

begin
  proc0;
  proc1( 1 );
  proc2( 21, 22 );
  ? forward_func0;
  ? forward_func1( 31 );
  ? forward_func2( 31, 32 );
end;

-- Programming by Contract

declare
  validate_error : exception;

  subtype long_string is string
    affirm
       raise validate_error with "too short" when strings.length( long_string ) < 5;
    end affirm;
  ls : long_string;
  ls2 : long_string := "fooxx";
  ls3 : constant long_string := "fooxx";
  pragma assumption( used, ls2 );
  pragma assumption( used, ls3 );

  type long_string2 is new string
    affirm
       raise validate_error with "too short" when strings.length( long_string2 ) < 5;
    end affirm;
  ls4 : long_string2;

  type small_int is new integer
    affirm
      if small_int < 0 or small_int > 10 then
         raise validate_error;
      end if;
    end affirm;

  procedure pbc_test( i1 : in out small_int ) is
  begin
    i1 := 5;
  end pbc_test;

  si : small_int;

  procedure pbc_test2( i2 : in small_int ) is
  begin
    si := i2;
  end pbc_test2;

  type als is array( 1..3 ) of long_string;
  als1 : array( 1..3 ) of long_string;
  als2 : als;

  type rls is record
     ls : long_string;
  end record;
  rls1 : rls;

  subtype mod_256 is integer
    affirm
      if mod_256 not in 0..255 then
        mod_256 := @ mod 256;
      end if;
    end affirm;

  unsigned_byte : mod_256;

begin
  ls := "fooxx";
  s := string( long_string( "fooxx" ) );
  ls4 := "fooxx";
  pbc_test( si );
  pbc_test2( 5 );
  als1(1) := "fooxx";
  als2(2) := "fooxx";
  rls1.ls := "fooxx";
  unsigned_byte := 416;
  pragma assert( unsigned_byte = 160 );
end;

-- A particular case of side-effect testing: the integer i
-- should not be flagged as a side-effect when the assignment
-- is within a larger expression

declare
  i : integer := 0;

  function test_f1 return boolean is
  begin
    i := i + 1;
    return true;
  end test_f1;
begin
  ? test_f1;
end;

-- I encountered a big where top-level resources threw an exception...
diropid : directory_operations.dir_type_id;

loop -- non-scope block
  directory_operations.open ( diropid, "." );
  directory_operations.close( diropid );
  exit when true;
end loop;

begin -- scoped block
  directory_operations.open ( diropid, "." );
  directory_operations.close( diropid );
end;

-- Logs package

declare
  s : string;
  log : constant string := "./testlog.log";
  lvl : logs.log_level;
begin
  if files.exists( log ) then
     rm "$log";
  end if;

  logs.open( log, log_mode.file );
  logs.info("a test");
  pragma assert( logs.is_open );
  pragma assert( logs.mode = log_mode.file );
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "a test" ) > 0 );
  pragma assert( strings.index( s, ":INFO:" ) > 0 );
  pragma assert( not logs.is_rotating );
  pragma assert( not logs.is_open );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.info("a ") @ ("test");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "a test" ) > 0 );
  pragma assert( strings.index( s, ":INFO:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.info("a ") @ ("test ") @ ("again");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "a test again" ) > 0 );
  pragma assert( strings.index( s, ":INFO:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file, 80 );
  pragma assert( logs.width = 80 );
  logs.info("indent test");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "   :indent test" ) > 0 );
  pragma assert( strings.index( s, ":INFO:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.info("clean:test");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, ":clean[# 58]test" ) > 0 );
  pragma assert( strings.index( s, ":INFO:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file, 80 );
  logs.level_begin( lvl );
  logs.info("level test");
  logs.level_end( lvl );
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "   :level test" ) > 0 );
  pragma assert( strings.index( s, ":INFO:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.warning("a test");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "a test" ) > 0 );
  pragma assert( strings.index( s, ":WARNING:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.warning("a ") @ ("test");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "a test" ) > 0 );
  pragma assert( strings.index( s, ":WARNING:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.warning("a ") @ ("test ") @ ("again");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "a test again" ) > 0 );
  pragma assert( strings.index( s, ":WARNING:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.error("a test");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "a test" ) > 0 );
  pragma assert( strings.index( s, ":ERROR:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.error("a ") @ ("test");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "a test" ) > 0 );
  pragma assert( strings.index( s, ":ERROR:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.error("a ") @ ("test ") @ ("again");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "a test again" ) > 0 );
  pragma assert( strings.index( s, ":ERROR:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.ok("a test");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "a test" ) > 0 );
  pragma assert( strings.index( s, ":OK:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.ok("a ") @ ("test");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "a test" ) > 0 );
  pragma assert( strings.index( s, ":OK:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.ok("a ") @ ("test ") @ ("again");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "a test again" ) > 0 );
  pragma assert( strings.index( s, ":OK:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.error("repeat test"); logs.error("repeat test");
  logs.error("a different line");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "repeat test" ) > 0 );
  pragma assert( strings.index( s, "a different line" ) > 0 );
  pragma assert( strings.index( s, "repeated" ) = 0 );
  pragma assert( strings.index( s, ":ERROR:" ) > 0 );
  rm "$log";

  logs.open( log, log_mode.file );
  logs.error("repeat test"); logs.error("repeat test"); logs.error("repeat test");
  logs.error("a different line");
  logs.close;
  s := `cat "$log"`;
  pragma assert( strings.index( s, "repeat test" ) > 0 );
  pragma assert( strings.index( s, "a different line" ) > 0 );
  pragma assert( strings.index( s, "repeated" ) > 0 );
  pragma assert( strings.index( s, ":ERROR:" ) > 0 );
  rm "$log";

  logs.open( "", log_mode.stderr );
  logs.info("stderr test");
  logs.close;
  pragma assert( not files.exists( log ) );

  logs.open( log, log_mode.echo );
  logs.info("stderr test");
  logs.close;
  pragma assert( files.exists( log ) );
  rm "$log";

  logs.rotate_begin;
  pragma assert( logs.is_rotating );
  logs.rotate_end;
  pragma assert( not logs.is_rotating );
end;

-- Pragmas

pragma assumption( used, i );
pragma assumption( written, i );

-- Types derived from parameterized types
-- Types with a resource must be limited.

declare
  -- the basic type
  type list_type is new limited doubly_linked_lists.list( string );

  -- Derrived types
  type list_type2 is new limited list_type;
  list : list_type;
  list2 : list_type2;

  subtype list_type3 is limited list_type;
  list3 : list_type3;
  subtype list_type4 is limited list_type3;
  list4 : list_type4;
  subtype list_type5 is limited list_type2;
  list5 : list_type5;

  -- User Functions for each type

  function len( lst : in out list_type ) return containers.count_type is
  begin
    return doubly_linked_lists.length( lst );
  end len;

  procedure free( lst : in out list_type ) is
  begin
    doubly_linked_lists.clear( lst );
  end free;

  function len2( lst : in out list_type2 ) return containers.count_type is
  begin
    return doubly_linked_lists.length( lst );
  end len2;

  procedure free2( lst : in out list_type2 ) is
  begin
    doubly_linked_lists.clear( lst );
  end free2;

  function len3( lst : in out list_type3 ) return containers.count_type is
  begin
    return doubly_linked_lists.length( lst );
  end len3;

  procedure free3( lst : in out list_type3 ) is
  begin
    doubly_linked_lists.clear( lst );
  end free3;

  function len4( lst : in out list_type4 ) return containers.count_type is
  begin
    return doubly_linked_lists.length( lst );
  end len4;

  procedure free4( lst : in out list_type4 ) is
  begin
    doubly_linked_lists.clear( lst );
  end free4;

  function len5( lst : in out list_type5 ) return containers.count_type is
  begin
    return doubly_linked_lists.length( lst );
  end len5;

  procedure free5( lst : in out list_type5 ) is
  begin
    doubly_linked_lists.clear( lst );
  end free5;

begin
  doubly_linked_lists.append( list, "foobar" );
  pragma assert( doubly_linked_lists.length( list ) =  len( list ) );
  free( list );
  pragma assert( doubly_linked_lists.length( list ) =  0 );
  doubly_linked_lists.append( list2, "foobar" );
  pragma assert( doubly_linked_lists.length( list2 ) =  len2( list2 ) );
  free2( list2 );
  pragma assert( doubly_linked_lists.length( list2 ) =  0 );
  doubly_linked_lists.append( list3, "foobar" );
  pragma assert( doubly_linked_lists.length( list3 ) =  len3( list3 ) );
  free3( list3 );
  pragma assert( doubly_linked_lists.length( list3 ) =  0 );
  doubly_linked_lists.append( list4, "foobar" );
  pragma assert( doubly_linked_lists.length( list4 ) =  len4( list4 ) );
  free4( list4 );
  pragma assert( doubly_linked_lists.length( list4 ) =  0 );
  doubly_linked_lists.append( list5, "foobar" );
  pragma assert( doubly_linked_lists.length( list5 ) =  len5( list5 ) );
  free5( list5 );
  pragma assert( doubly_linked_lists.length( list5 ) =  0 );
end;

-- constant specifications

declare
  c1 : constant integer;

  c1 : constant integer := 1;

  -- Array type declared but never used
  type atype is array(1..1) of integer;
  c7 : constant atype;
  c7 : constant atype := (1);

  -- Constant enumerated specification

  c8 : constant boolean;
  c8 : constant boolean := true;

  -- Constant record specification

  type rtype is record
     i : integer;
  end rtype;

  c9 : constant rtype;
  c9 : constant rtype := (9);

  -- Constant record fields (a strange case)

  type const_int is new constant integer;

  type rtype2 is record
     i : const_int;
  end record;

  c11 : rtype2;
  c11.i : const_int := 11;

  -- A constant record type

  type rtype3 is constant record
    i : integer;
  end record;

  c12 : rtype3 := (12);

  -- Constant array

  type t10 is constant array(1..1) of integer;
  c10 : t10;
  c10 : t10 := (10);

begin
  pragma assert( c1 = 1 );
  pragma assert( c7(1) = 1 );
  pragma assert( c8 = true );
  pragma assert( c9.i= 9 );
  pragma assert( c10(1) = 10 );
  pragma assert( c11.i = 11 );
  pragma assert( c12.i = 12 );
end;

-- Pragma ada_95 tests

pragma ada_95;

-- This is a comment

ada95_s : string;
ada95_i : integer := 0;

ada95_s := "assigned"; -- suppress unused var warning

i := (1 + 1 ) / 1;
pragma assert( i=2 );

if i = 1 then
   null;
elsif i = 2 then
   null;
else
   null;
end if;

i := 1;
while i < 5 loop
   i := i+1;
   exit when i = 3;
end loop;
pragma assert( i=3 ); -- ada_95 while

i := 99;
ada95_i := 0;
for i in 1..10 loop
    ada95_i := ada95_i + 1;
end loop;
pragma assert( i=99 ); -- ada_95 for
pragma assert( ada95_i=10 );

-- end of tests (put new tests above the pragma ada_95 section
-- unless that's what you're testing!)

trace( false );
new_line;
put_line( "CONGRATULATIONS! SPARFORTE HAS SUCCESSFULLY RUN THIS TEST SCRIPT" );

-- VIM editor formatting instructions
-- vim: ft=spar

