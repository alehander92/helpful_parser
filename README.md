# helpful_parser

A helpful parser with love.

Also, very WIP, just usable for a toy project.

## dsl

Currently: RULES are defined as data structures.

A possible grammar dsl is

```nim
Program   => (TopLevel @nl)*
TopLevel  => FunctionDef / Signature / ComplexitySignature / Expr
FunctionDef => Name "(" Args ")" ":" Code
Signature => join(Type "->")
ComplexitySignature => join(Name _ "->" _) _ "->" _ BigO
Code      => @indent Expr* @dedent
Type      => Name / Index
BigO#     => "O" "[" Expr "]"
Index     => Expr "[" Expr "]"
Expr      => LeftExpr RightExpr
LeftExpr  => IfNode / ForRange / ForIn / ReturnNode / Assign / Declaration / InfixOp / Name / Number
RightExpr => RightCall / @nothing
RightCall => "(" CallArgs ")"
RightCall: Call
IfNode    => "if" _ Expr ":" Code
ForRange  => "for" _ Expr _ "in" _ Expr _ "..<" _ Expr ":" Code
indent: 2
```
We support indented source assuming certain rules.

## Plans

Improve the rules and error reporting because of usage in pseudo-lang or for other goals.

Thanks God this works even for the current rules: I am not sure if I'll be able to continue until this becomes a useful lib for others, but I'd try to at least improve it a bit.
