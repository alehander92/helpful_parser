# helpful

var source = """
Program   => (TopLevel @nl)*
TopLevel  => FunctionDef / Signature / ComplexitySignature / Expr
FunctionDef => Name "(" Args ")" ":" Code
Signature => join(Type "->")
ComplexitySignature => join(Name _ "->" _) _ "->" _ BigO
Code      => @indent Expr* @dedent
Type      => Name / Index
BigO      => "O" "[" Expr "]"
Index     => Expr "[" Expr "]"
Expr      => LeftExpr RightExpr
LeftExpr  => IfNode / ForRange / ForIn / ReturnNode / Assign / Declaration / InfixOp / Name / Number
RightExpr => RightCall / @nothing
RightCall => "(" CallArgs ")"
RightCall: Call
IfNode    => "if" _ Expr ":" Code
ForRange  => "for" _ Expr _ "in" _ Expr _ "..<" _ Expr ":" Code
"""

type
  Rule* = object

left:
  Name('a')
right:
  CallArgs:
    left:
      Number(2)
    right:
      Nothing
    left:
      Number(4)
    right:
      Nothing

LeftExpr + RightCall => Call

Call(
  @[Name('a'), CallArgs(Number)])

Number(2)
Number(4)


(IfNode / Assign / Name / Number) MaybeCall

MaybeCall => ("(" CallArgs ")") / ""


using
  start: int
  ctx: Context

proc parseJoin(start, ctx): (Node, int, bool) =
  var children: seq[Node]
  var i = start
  var success = true
  var child0: Node
  var success0 = false
  var i0 = i
  var child1: Node
  var success1 = false
  var i1 = i
    
  while true:
    (child0, i0, success0) = parseTopLevel(i, ctx)
    if success0:
      (child1, i1, success1) = parseLit("->", i0, ctx)
      if success1:
        children.add(child0)
        i = i1
      else:
        success = true
        break
    else:
      success = false
      break

  result = (Program.init(children), i, success)

proc parseTopLevel(start, ctx): (Node, int, bool) =
  var child: Node
  var i = start
  var success = false
  (child, i, success) = parseFunctionDef(start, ctx)
  if not success:
    (child, i, success) = parseSignature(start, ctx)
  result = (child, i, success)

proc parseProgram(start, ctx): (Node, int, bool) =
  var children: seq[Node]
  var i = start
  var child0: Node
  var success0 = false
  var i0 = i
  var child1: Node
  var success1 = false
  var i1 = i
    
  while true:
    (child0, i0, success0) = parseTopLevel(i, ctx)
    if success0:
      (child1, i1, success1) = parseNl(i0, ctx)
      if success1:
        children.add(child0)
        i = i1
      else:
        break
    else:
      break

  result = (Program.init(children), i, true)
