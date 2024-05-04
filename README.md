# Trion
Trion is an assembler (with some disassembling capabilities) designed to be used with the Raspberry Pico (RP2040) microcontroller.

## Assembly Syntax
Trion's assembly syntax is mostly similar to that of other assemblers, with slight modifications to simplify the parsing step.

An assembly file (typically but not necessarily `*.asm`) consists of a number of statements, which can be any of the following:
- Directives: `"." <name> [<argument> {"," <argument>}] ";"`
- Labels: `<name> ":"`
- Instructions `<name>["." <flags>] [<argument> {"," <argument>}] ";"`

The number and types of arguments accepted depend on the directive or instruction used, but parsing may continue if they are wrong.

## Arguments
Directives and instructions often take arguments that control their behavior. Atomic arguments are:
- Immediate: Integer values encoded in binary, octal, decimal or hexadecimal format in the source code.
  The prefixes for binary, octal and hexadecimal are `"0b"`, `"0o"` and `"0x"`  respectively.
- Character: Effectively an immediate but formatted as a unicode code point delimited by single quotes.
- Identifier: Names that can refer to registers or constants.
  Valid characters are `"A"` to `"Z"` (case-insensitive) and `"_"`, after the first `"$"`, `"."`, `"@"` or `"0"` to `"9"` are also valid.
- String: A sequence of unicode code points delimited by double quotes.

Further, trion supports a number of numeric operations (in order of highest precedence first):
- Negation `"-" <arg>`, Bitwise not `"!" <arg>`
- Multiplication `<lhs> "*" <rhs>`, Division `<lhs> "/" <rhs>`, Modulo `<lhs> "%" <rhs>`
- Addition `<lhs> "+" <rhs>`, Subtraction `<lhs> "-" <rhs>`
- Left shift (logical) `<lhs> "<<" <rhs>`, Right shift (logical) `<lhs> ">>" <rhs>`
- Binary and `<lhs> & <rhs>`
- Binary xor `<lhs> ^ <rhs>`
- Binary or `<lhs> | <rhs>`

Finally, there are a number of special operators:
- Address (`"[" <arg> "]"`): Indicates that the processor should access the memory location instead of the numeric value.
- Sequence (`"{" [<argument> {, <argument>}] "}"`): A generic collection of arguments, typically a register set.
- Function (`<name> "(" [<argument> {"," <argument>}] ")"`): A general-purpose to perform computations on values of any type.

As with regular mathematics, it is always valid to wrap any argument in parentheses to affect evaluation order.
For numeric operations, though, all arguments must evaluate to a register or numeric value to be valid.
In addition, it is an error for the result of a mathematical operation to overflow the constant's integer limit (64-bit signed).

## Constants
Trion utilizes compile-time constants to enable simplification and source code reuse.
Constants must not have the same name as any register in the current instruction set.
Constants are signed 64-bit integers to comfortably encompass the RP2040's 32-bit address and register size.
Labels are themselves constants, with a value equivalent to their location, thus arbitrary numeric arguments can be used as branch/load targets.  
Use of constants is dictated by scope: with the exception of importing/exporting, all directives and instructions use local constants.
It is possible to declare a constant after it is used (especially labels) but search won't escape to parent scope unless the constant is deferred.

## Assembling
Usage: `cargo run assemble <input_file.asm> [<ouput_file.uf2>]`  
The assembler is used to turn human-readable assembly source code into a UF2 container recognized by the RP2040's startup routine.
As such, it is possible for the assembler to directly upload code onto the device via USB by choosing an output path within the device's drive.
The output file argument is optional however, if omitted the assembler will proceed as normal but will not save the final output.
This is intended for syntax validation or testing purposes.

### Directives
- `".addr" <addr> ";"`: Changes the current output address to `addr` (where data and instructions are written to).
- `".align" <alignment> ";"`: Aligns the current address to `alignment` bytes.
  The empty space (if any) is filled with `0xBE` (the opcode for the `BKPT` instruction)
- `".const" <name> "," <value> ";"`: Creates a local constant with the given name (identifier) and value.
  `value` must evaluate to a numeric value containing no deferred constants.
- `".du8" <value> ";"`, `".du16" <value> ";"`, `".du32" <value> ";"`: Outputs an unsigned integer value, which must be in the specified range.
- `".dhex" <value> ";"`: Decodes a hexadecimal string and outputs the resulting bytes.
- `".dstr" <value> ";"`: Outputs a string as-is (without padding, length prefix or terminal character).
- `".dfile" <path> ";"`: Outputs the named file as-is, the `path` must be a string relative to the path of the current file.
- `".global" <name> ";"`: Declares a deferred constant `name` which is exported at the end of the local scope.
  It is valid for the constant to already exist locally, including if deferred. In the former case, it is exported immediately.
- `".import" <name> ";"`: Imports the (possibly deferred) constant `name` from the parent scope to the local scope.
- `".export" <name> ";"`: Exports the existing, non-deferred constant `name` from the local scope to the parent scope.
- `".include" <path> ";"`: Assembles the file at the relative `path` (must be a string) and outputs the result at the current address.
  The file is assembled with its own local scope, but it can import/export constants from/to the current file's local scope.

Directives (and instructions) which can write data can only be used after an `.addr` has been set.
The assembler does not require proper alignment, but the RP2040 will not be able to access data or execute instructions which aren't aligned.

### Instructions
Trion currently supports only the ARMv6-M instruction set (which is used by the RP2040).
Instructions have the same names but, unlike ARM's UAL, require a terminal semicolon.
Flags are only supported (and in fact required) for the `UDF` instruction (which has narrow and wide representations).

## Disassembling
Usage: `cargo run disassemble <input_file.bin>`  
The disassembler is not the exact inverse of the assembler. For example, it requires as input a binary file containing only the opcodes, not a UF2 container.
Therefore, and because assembling strips label names, the disassembler will assume the binary is mounted at addess `0x2000000` (the start of SRAM on the
RP2040) and generate hexadecimal label names based on these addresses.
It currently only follows labels to other code, and won't generate any directives for relevant data items (such as with an immediate `LDR`).
