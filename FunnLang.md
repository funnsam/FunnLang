## Syntax:
```c
// Comment
// <- single line comments
/* <- start of multiline comment
and the end of it -> */

// Types
   u8  i8
   u16 i16
   u32 i32
   u64 i64
// ^^^ ^^^ only avalible if (n) is more than target bits
   uint int
// ^^^^ ^^^ (target) bits int / uint

   str  // Same as char arrays

// Type: alias
type char alias: u8;
type bool alias: u8;

// Type: arrays
<type>[<size>]
u8[123]

// Strings and chars
"A string" // <- a string
'a'        // <- a char

// Variables
var <var_name>: <var_type> = <init_val>;

// Functions
func <func_name>(<args..>) <ret_type> {
    <func_body>
}

<func_name>(<args..>);

// Conditions and flow control
if (<cond>) {
    <body>
// } else if (<cond>) {
// ...
// } else {
//     <body>
}
while (<cond>) {
    <body>
}
for (<iter_var>: <start>..<end>) {
    <body>
}

// Assembly blocks
asm "<asm_code_in_here>";
```