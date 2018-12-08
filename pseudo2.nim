import strutils, sequtils, sets, pseudo, macros

type
  Context = ref object
    input: string
    depth: int

proc toSet(a: HSlice[char, char]): set[char] =
  for b in a:
    result.incl(b)

# similar to substrEq in strutils

proc substr(a: string, b: int, c: set[char] = {}): string =
  if b >= a.len:
    return ""
  var i = b
  while i < a.len:
    let d = a[i]
    if d notin c:
      break
    else:
      result.add(d)
    i += 1

var nameSymbols = toSet('a'..'z') + {'_'}
var numberSymbols = toSet('0' .. '9')
var typeSymbols = toSet('A'..'Z') + toSet('a'..'z') + {'_'}
var operatorSymbols = {'+', '-', '*', '^'}

proc substrEq(a: string, b: int, c: string): bool =
  if b + c.len > a.len:
    return false
  else:
    for i, d in c:
      if a[b + i] != d:
        return false
    echo b, ": ", c, " ", "ok"
    return true


macro log(name: untyped): untyped =
  let e = newLit($name)
  quote:
    echo repeat("  ", ctx.depth), "visit: ", start, " ", `e`
    ctx.depth += 1

macro finalLog(name: untyped): untyped =
  let e = newLit($name)
  quote:
    ctx.depth -= 1

proc parseLit(a: string, start: int, ctx: Context): (Node, int, bool) =
  log(lit)
  if ctx.input.substrEq(start, a):
    result = (nil, start + a.len, true)
  else:
    result = (nil, start, false)
  finalLog(lit)

proc parseSet(a: set[char], start: int, ctx: Context): (Node, int, bool) =
  log(set)
  let parsed = substr(ctx.input, start, a)
  result = (nil, start + parsed.len, true)
  finalLog(set)

proc parseIndent(start: int, ctx: Context): (Node, int, bool) =
  parseLit("###INDENT###", start, ctx)

proc parseDedent(start: int, ctx: Context): (Node, int, bool) =
  parseLit("###DEDENT###", start, ctx)

proc parseWs(start: int, ctx: Context): (Node, int, bool) =
  parseSet({' ', '\t'}, start, ctx)

proc parseNl(start: int, ctx: Context): (Node, int, bool) =
  parseSet({'\L'}, start, ctx)

proc parseNothing(start: int, ctx: Context): (Node, int, bool) =
  log(nothing)
  result = (nil, start, true)
  finalLog(nothing)

proc load(input: string, indent: int): string =
  let lines = input.splitLines.mapIt(it.strip(leading=false))
  result = ""
  var current = 0
  for a, line in lines:
    if line.strip.len == 0:
      result.add("\n")
    else:
      var length = substr(line, 0, {' '}).len
      var newIndent = length div indent
      if newIndent > current + 1:
        raise newException(ValueError, "INDENT " & $a)
      elif newIndent == current + 1:
        result.add("###INDENT###" & line[length .. ^1] & "\n")
      elif newIndent == current:
        result.add(line[length .. ^1] & "\n")
      else:
        for b in newIndent ..< current:
          result.add("###DEDENT###\n")
        result.add(line[length .. ^1] & "\n")
      current = newIndent
  if current > 0:
    for b in 0 ..< current:
      result.add("###DEDENT###\n")
proc parseLeftExpr(start: int, ctx: Context): (Node, int, bool)
proc parseComplexityInfix(start: int, ctx: Context): (Node, int, bool)
proc parseDeclaration(start: int, ctx: Context): (Node, int, bool)
proc parseCallArgs(start: int, ctx: Context): (Node, int, bool)
proc parseType(start: int, ctx: Context): (Node, int, bool)
proc parseBigM(start: int, ctx: Context): (Node, int, bool)
proc parseExpr(start: int, ctx: Context): (Node, int, bool)
proc parseBigO(start: int, ctx: Context): (Node, int, bool)
proc parseArgs(start: int, ctx: Context): (Node, int, bool)
proc parseComplexityA(start: int, ctx: Context): (Node, int, bool)
proc parseOperator(start: int, ctx: Context): (Node, int, bool)
proc parseDeclarationName(start: int, ctx: Context): (Node, int, bool)
proc parseRightExpr(start: int, ctx: Context): (Node, int, bool)
proc parseRightInfix(start: int, ctx: Context): (Node, int, bool)
proc parseReturnNode(start: int, ctx: Context): (Node, int, bool)
proc parseComplexityExpression(start: int, ctx: Context): (Node, int, bool)
proc parseProgram(start: int, ctx: Context): (Node, int, bool)
proc parseTypename(start: int, ctx: Context): (Node, int, bool)
# FAITH
proc parseNumber(start: int, ctx: Context): (Node, int, bool)
proc parseIndex(start: int, ctx: Context): (Node, int, bool)
proc parseForRange(start: int, ctx: Context): (Node, int, bool)
proc parseTopLevel(start: int, ctx: Context): (Node, int, bool)
proc parseAssign(start: int, ctx: Context): (Node, int, bool)
proc parseRightCall(start: int, ctx: Context): (Node, int, bool)
proc parseFunctionDef(start: int, ctx: Context): (Node, int, bool)
proc parseComplexityRule(start: int, ctx: Context): (Node, int, bool)
proc parseComplexitySignature(start: int, ctx: Context): (Node, int, bool)
proc parseLocal0(start: int, ctx: Context): (seq[Node], int, bool)
proc parseCode(start: int, ctx: Context): (Node, int, bool)
proc parseSignature(start: int, ctx: Context): (Node, int, bool)
proc parseSimple(start: int, ctx: Context): (Node, int, bool)
proc parseName(start: int, ctx: Context): (Node, int, bool)
proc parse*(input: string): Node

proc parseLeftExpr(start: int, ctx: Context): (Node, int, bool) =
  log(LeftExpr)
  result = parseDeclaration(start, ctx)
  if not result[2]:
    result = parseAssign(start, ctx)
  if not result[2]:
    result = parseForRange(start, ctx)
  if not result[2]:
    result = parseReturnNode(start, ctx)
  if not result[2]:
    result = parseName(start, ctx)
  if not result[2]:
    result = parseNumber(start, ctx)
  finalLog(LeftExpr)



proc parseComplexityInfix(start: int, ctx: Context): (Node, int, bool) =
  log(ComplexityInfix)
  var children: seq[Node]
  var child: Node
  var i = start
  var success = false
  var localChildren: seq[Node]
  
  (child, i, success) = parseSimple(i, ctx)
  if not success:
  
    finalLog(ComplexityInfix);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseWs(i, ctx)
  if not success:
  
    finalLog(ComplexityInfix);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseOperator(i, ctx)
  if not success:
  
    finalLog(ComplexityInfix);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseWs(i, ctx)
  if not success:
  
    finalLog(ComplexityInfix);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseComplexityExpression(i, ctx)
  if not success:
  
    finalLog(ComplexityInfix);return (nil, start, false)
  if not child.isNil: children.add(child)
  result = (ComplexityInfix.init(children), i, true)
  finalLog(ComplexityInfix)



proc parseDeclaration(start: int, ctx: Context): (Node, int, bool) =
  log(Declaration)
  var children: seq[Node]
  var child: Node
  var i = start
  var success = false
  var localChildren: seq[Node]
  
  (child, i, success) = parseDeclarationName(i, ctx)
  if not success:
  
    finalLog(Declaration);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseWs(i, ctx)
  if not success:
  
    finalLog(Declaration);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseAssign(i, ctx)
  if not success:
  
    finalLog(Declaration);return (nil, start, false)
  if not child.isNil: children.add(child)
  result = (Declaration.init(children), i, true)
  finalLog(Declaration)



proc parseCallArgs(start: int, ctx: Context): (Node, int, bool) =
  log(CallArgs)
  var children: seq[Node]
  var i = start
  var success = true
  
  var i0 = i
  var child0: Node
  var success0 = false
  
  var i1 = i
  var child1: Node
  var success1 = false
  
  var i2 = i
  var child2: Node
  var success2 = false
  
  var i3 = i
  var child3: Node
  var success3 = false
  
  while true:

    (child0, i0, success0) = parseExpr(i3, ctx)
    if success0:
    
      children.add(child0)

      i = i0

      (child1, i1, success1) = parseWs(i0, ctx)
      if success1:
      
        discard
        (child2, i2, success2) = parseLit(",", i1, ctx)
        if success2:
        
          discard
          (child3, i3, success3) = parseWs(i2, ctx)
          if success3:
          
            discard
          else:
            break
        else:
          break
      else:
        break
    else:
      break

  result = (CallArgs.init(children), i, success)
  finalLog(CallArgs)



proc parseType(start: int, ctx: Context): (Node, int, bool) =
  log(Type)
  result = parseTypename(start, ctx)
  if not result[2]:
    result = parseIndex(start, ctx)
  finalLog(Type)



proc parseBigM(start: int, ctx: Context): (Node, int, bool) =
  log(BigM)
  var children: seq[Node]
  var child: Node
  var i = start
  var success = false
  var localChildren: seq[Node]
  
  (child, i, success) = parseLit("M[", i, ctx)
  if not success:
  
    finalLog(BigM);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseComplexityExpression(i, ctx)
  if not success:
  
    finalLog(BigM);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseLit("]", i, ctx)
  if not success:
  
    finalLog(BigM);return (nil, start, false)
  if not child.isNil: children.add(child)
  result = (BigM.init(children), i, true)
  finalLog(BigM)



proc parseExpr(start: int, ctx: Context): (Node, int, bool) =
  log(expr)
  var child0: Node
  var i = start
  var success0 = false
  var child1: Node
  var success1 = false

  (child0, i, success0) = parseLeftExpr(start, ctx)
  if success0:
    (child1, i, success1) = parseRightExpr(i, ctx)
    if success1:
      if child1.isNil:
        result = (child0, i, true)
      else:
        let children = @[child0].concat(child1.children)
        let node = case child1.kind:
          of RightInfix: InfixOp.init(children)
          of RightCall: Call.init(children)
          else: nil

        result = (node, i, true)
      return
  result = (nil, start, false)
  finalLog(expr)
  


proc parseBigO(start: int, ctx: Context): (Node, int, bool) =
  log(BigO)
  var children: seq[Node]
  var child: Node
  var i = start
  var success = false
  var localChildren: seq[Node]
  
  (child, i, success) = parseLit("O[", i, ctx)
  if not success:
  
    finalLog(BigO);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseComplexityExpression(i, ctx)
  if not success:
  
    finalLog(BigO);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseLit("]", i, ctx)
  if not success:
  
    finalLog(BigO);return (nil, start, false)
  if not child.isNil: children.add(child)
  result = (BigO.init(children), i, true)
  finalLog(BigO)



proc parseArgs(start: int, ctx: Context): (Node, int, bool) =
  log(Args)
  var children: seq[Node]
  var i = start
  var success = true
  
  var i0 = i
  var child0: Node
  var success0 = false
  
  var i1 = i
  var child1: Node
  var success1 = false
  
  var i2 = i
  var child2: Node
  var success2 = false
  
  var i3 = i
  var child3: Node
  var success3 = false
  
  while true:

    (child0, i0, success0) = parseName(i3, ctx)
    if success0:
    
      children.add(child0)

      i = i0

      (child1, i1, success1) = parseWs(i0, ctx)
      if success1:
      
        discard
        (child2, i2, success2) = parseLit(",", i1, ctx)
        if success2:
        
          discard
          (child3, i3, success3) = parseWs(i2, ctx)
          if success3:
          
            discard
          else:
            break
        else:
          break
      else:
        break
    else:
      break

  result = (Args.init(children), i, success)
  finalLog(Args)



proc parseComplexityA(start: int, ctx: Context): (Node, int, bool) =
  log(ComplexityA)
  result = parseComplexityExpression(start, ctx)
  if not result[2]:
    result = parseBigO(start, ctx)
  if not result[2]:
    result = parseBigM(start, ctx)
  finalLog(ComplexityA)



proc parseOperator(start: int, ctx: Context): (Node, int, bool) =
  log(operator)
  let sub = ctx.input.substr(start, operatorSymbols)
  
  if sub.len == 0:
    result = (nil, start, false)
  else:
    result = (Operator.init(sub), start + sub.len, true)
  finalLog(name)
  


proc parseDeclarationName(start: int, ctx: Context): (Node, int, bool) =
  log(DeclarationName)
  result = parseLit("let", start, ctx)
  result[0] = DeclarationName.init("let")
  if not result[2]:
    result = parseLit("var", start, ctx)
    result[0] = DeclarationName.init("var")
  finalLog(DeclarationName)



proc parseRightExpr(start: int, ctx: Context): (Node, int, bool) =
  log(RightExpr)
  result = parseRightCall(start, ctx)
  if not result[2]:
    result = parseRightInfix(start, ctx)
  if not result[2]:
    result = parseNothing(start, ctx)
  finalLog(RightExpr)



proc parseRightInfix(start: int, ctx: Context): (Node, int, bool) =
  log(RightInfix)
  var children: seq[Node]
  var child: Node
  var i = start
  var success = false
  var localChildren: seq[Node]
  
  (child, i, success) = parseWs(i, ctx)
  if not success:
  
    finalLog(RightInfix);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseOperator(i, ctx)
  if not success:
  
    finalLog(RightInfix);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseWs(i, ctx)
  if not success:
  
    finalLog(RightInfix);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseExpr(i, ctx)
  if not success:
  
    finalLog(RightInfix);return (nil, start, false)
  if not child.isNil: children.add(child)
  result = (RightInfix.init(children), i, true)
  finalLog(RightInfix)



proc parseReturnNode(start: int, ctx: Context): (Node, int, bool) =
  log(ReturnNode)
  var children: seq[Node]
  var child: Node
  var i = start
  var success = false
  var localChildren: seq[Node]
  
  (child, i, success) = parseLit("return ", i, ctx)
  if not success:
  
    finalLog(ReturnNode);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseWs(i, ctx)
  if not success:
  
    finalLog(ReturnNode);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseExpr(i, ctx)
  if not success:
  
    finalLog(ReturnNode);return (nil, start, false)
  if not child.isNil: children.add(child)
  result = (ReturnNode.init(children), i, true)
  finalLog(ReturnNode)



proc parseComplexityExpression(start: int, ctx: Context): (Node, int, bool) =
  log(ComplexityExpression)
  result = parseComplexityInfix(start, ctx)
  if not result[2]:
    result = parseName(start, ctx)
  if not result[2]:
    result = parseNumber(start, ctx)
  finalLog(ComplexityExpression)



proc parseProgram(start: int, ctx: Context): (Node, int, bool) =
  log(Program)
  var children: seq[Node]
  var i = start
  var success = true
  
  var i0 = i
  var child0: Node
  var success0 = false
  
  var i1 = i
  var child1: Node
  var success1 = false
  
  while true:

    (child0, i0, success0) = parseToplevel(i1, ctx)
    if success0:
    
      discard
      (child1, i1, success1) = parseNl(i0, ctx)
      if success1:
      
        children.add(child0)

        i = i1

      else:
        break
    else:
      break

  result = (Program.init(children), i, success)
  finalLog(Program)



proc parseTypename(start: int, ctx: Context): (Node, int, bool) =
  log(typename)
  let sub = ctx.input.substr(start, typeSymbols)
  
  if sub.len == 0:
    result = (nil, start, false)
  else:
    result = (Typename.init(sub), start + sub.len, true)
  finalLog(name)
  


# FAITH
proc parseNumber(start: int, ctx: Context): (Node, int, bool) =
  log(number)
  let sub = ctx.input.substr(start, numberSymbols)
  
  if sub.len == 0:
    result = (nil, start, false)
  else:
    result = (Number.init(sub.parseInt), start + sub.len, true)
  finalLog(name)
  


proc parseIndex(start: int, ctx: Context): (Node, int, bool) =
  log(Index)
  var children: seq[Node]
  var child: Node
  var i = start
  var success = false
  var localChildren: seq[Node]
  
  (child, i, success) = parseTypename(i, ctx)
  if not success:
  
    finalLog(Index);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseLit("[", i, ctx)
  if not success:
  
    finalLog(Index);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseExpr(i, ctx)
  if not success:
  
    finalLog(Index);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseLit("]", i, ctx)
  if not success:
  
    finalLog(Index);return (nil, start, false)
  if not child.isNil: children.add(child)
  result = (Index.init(children), i, true)
  finalLog(Index)



proc parseForRange(start: int, ctx: Context): (Node, int, bool) =
  log(ForRange)
  var children: seq[Node]
  var child: Node
  var i = start
  var success = false
  var localChildren: seq[Node]
  
  (child, i, success) = parseLit("for ", i, ctx)
  if not success:
  
    finalLog(ForRange);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseWs(i, ctx)
  if not success:
  
    finalLog(ForRange);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseName(i, ctx)
  if not success:
  
    finalLog(ForRange);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseWs(i, ctx)
  if not success:
  
    finalLog(ForRange);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseLit("in ", i, ctx)
  if not success:
  
    finalLog(ForRange);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseWs(i, ctx)
  if not success:
  
    finalLog(ForRange);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseExpr(i, ctx)
  if not success:
  
    finalLog(ForRange);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseWs(i, ctx)
  if not success:
  
    finalLog(ForRange);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseLit("..<", i, ctx)
  if not success:
  
    finalLog(ForRange);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseWs(i, ctx)
  if not success:
  
    finalLog(ForRange);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseExpr(i, ctx)
  if not success:
  
    finalLog(ForRange);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseLit(":", i, ctx)
  if not success:
  
    finalLog(ForRange);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseCode(i, ctx)
  if not success:
  
    finalLog(ForRange);return (nil, start, false)
  if not child.isNil: children.add(child)
  result = (ForRange.init(children), i, true)
  finalLog(ForRange)



proc parseTopLevel(start: int, ctx: Context): (Node, int, bool) =
  log(TopLevel)
  result = parseFunctionDef(start, ctx)
  if not result[2]:
    result = parseSignature(start, ctx)
  if not result[2]:
    result = parseComplexityRule(start, ctx)
  if not result[2]:
    result = parseExpr(start, ctx)
  finalLog(TopLevel)



proc parseAssign(start: int, ctx: Context): (Node, int, bool) =
  log(Assign)
  var children: seq[Node]
  var child: Node
  var i = start
  var success = false
  var localChildren: seq[Node]
  
  (child, i, success) = parseName(i, ctx)
  if not success:
  
    finalLog(Assign);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseWs(i, ctx)
  if not success:
  
    finalLog(Assign);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseLit("=", i, ctx)
  if not success:
  
    finalLog(Assign);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseWs(i, ctx)
  if not success:
  
    finalLog(Assign);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseExpr(i, ctx)
  if not success:
  
    finalLog(Assign);return (nil, start, false)
  if not child.isNil: children.add(child)
  result = (Assign.init(children), i, true)
  finalLog(Assign)



proc parseRightCall(start: int, ctx: Context): (Node, int, bool) =
  log(RightCall)
  var children: seq[Node]
  var child: Node
  var i = start
  var success = false
  var localChildren: seq[Node]
  
  (child, i, success) = parseLit("(", i, ctx)
  if not success:
  
    finalLog(RightCall);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseCallArgs(i, ctx)
  if not success:
  
    finalLog(RightCall);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseLit(")", i, ctx)
  if not success:
  
    finalLog(RightCall);return (nil, start, false)
  if not child.isNil: children.add(child)
  result = (children[0], i, true)
  finalLog(RightCall)



proc parseFunctionDef(start: int, ctx: Context): (Node, int, bool) =
  log(FunctionDef)
  var children: seq[Node]
  var child: Node
  var i = start
  var success = false
  var localChildren: seq[Node]
  
  (child, i, success) = parseLit("def ", i, ctx)
  if not success:
  
    finalLog(FunctionDef);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseName(i, ctx)
  if not success:
  
    finalLog(FunctionDef);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseLit("(", i, ctx)
  if not success:
  
    finalLog(FunctionDef);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseArgs(i, ctx)
  if not success:
  
    finalLog(FunctionDef);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseLit("):", i, ctx)
  if not success:
  
    finalLog(FunctionDef);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseCode(i, ctx)
  if not success:
  
    finalLog(FunctionDef);return (nil, start, false)
  if not child.isNil: children.add(child)
  result = (FunctionDef.init(children), i, true)
  finalLog(FunctionDef)



proc parseComplexityRule(start: int, ctx: Context): (Node, int, bool) =
  log(ComplexityRule)
  var children: seq[Node]
  var child: Node
  var i = start
  var success = false
  var localChildren: seq[Node]
  
  (child, i, success) = parseLit("%", i, ctx)
  if not success:
  
    finalLog(ComplexityRule);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseComplexitySignature(i, ctx)
  if not success:
  
    finalLog(ComplexityRule);return (nil, start, false)
  if not child.isNil: children.add(child)
  result = (children[0], i, true)
  finalLog(ComplexityRule)



proc parseComplexitySignature(start: int, ctx: Context): (Node, int, bool) =
  log(ComplexitySignature)
  var children: seq[Node]
  var i = start
  var success = true
  
  var i0 = i
  var child0: Node
  var success0 = false
  
  var i1 = i
  var child1: Node
  var success1 = false
  
  while true:

    (child0, i0, success0) = parseComplexityA(i1, ctx)
    if success0:
    
      children.add(child0)

      i = i0

      (child1, i1, success1) = parseLit("->", i0, ctx)
      if success1:
      
        discard
      else:
        break
    else:
      break

  success = children.len > 0
  result = (ComplexitySignature.init(children), i, success)
  finalLog(ComplexitySignature)



proc parseLocal0(start: int, ctx: Context): (seq[Node], int, bool) =
  log(Local0)
  var children: seq[Node]
  var i = start
  var success = true
  
  var i0 = i
  var child0: Node
  var success0 = false
  
  var i1 = i
  var child1: Node
  var success1 = false
  
  while true:

    (child0, i0, success0) = parseExpr(i1, ctx)
    if success0:
    
      discard
      (child1, i1, success1) = parseNl(i0, ctx)
      if success1:
      
        children.add(child0)

        i = i1

      else:
        break
    else:
      break

  result = (children, i, success)
  finalLog(Local0)



proc parseCode(start: int, ctx: Context): (Node, int, bool) =
  log(Code)
  var children: seq[Node]
  var child: Node
  var i = start
  var success = false
  var localChildren: seq[Node]
  
  (child, i, success) = parseNl(i, ctx)
  if not success:
  
    finalLog(Code);return (nil, start, false)
  if not child.isNil: children.add(child)
  (child, i, success) = parseIndent(i, ctx)
  if not success:
  
    finalLog(Code);return (nil, start, false)
  if not child.isNil: children.add(child)
  (localChildren, i, success) = parseLocal0(i, ctx)
  children.add(localChildren)
  
  (child, i, success) = parseDedent(i, ctx)
  if not success:
  
    finalLog(Code);return (nil, start, false)
  if not child.isNil: children.add(child)
  result = (Code.init(children), i, true)
  finalLog(Code)



proc parseSignature(start: int, ctx: Context): (Node, int, bool) =
  log(Signature)
  var children: seq[Node]
  var i = start
  var success = true
  
  var i0 = i
  var child0: Node
  var success0 = false
  
  var i1 = i
  var child1: Node
  var success1 = false
  
  while true:

    (child0, i0, success0) = parseType(i1, ctx)
    if success0:
    
      children.add(child0)

      i = i0

      (child1, i1, success1) = parseLit("->", i0, ctx)
      if success1:
      
        discard
      else:
        break
    else:
      break

  success = children.len > 0
  result = (Signature.init(children), i, success)
  finalLog(Signature)



proc parseSimple(start: int, ctx: Context): (Node, int, bool) =
  log(Simple)
  result = parseName(start, ctx)
  if not result[2]:
    result = parseNumber(start, ctx)
  finalLog(Simple)



proc parseName(start: int, ctx: Context): (Node, int, bool) =
  log(name)
  let sub = ctx.input.substr(start, nameSymbols)
  
  if sub.len == 0:
    result = (nil, start, false)
  else:
    result = (Name.init(sub), start + sub.len, true)
  finalLog(name)
  


proc parse*(input: string): Node =
  var i = load(input, 2)
  echo i
  var ctx = Context(input: i)
  var res = parseProgram(0, ctx)
  if res[2]:
    res[0]
  else:
    echo "error"
    nil
