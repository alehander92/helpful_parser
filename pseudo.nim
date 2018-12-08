import macros, strutils, sequtils

type
  NodeKind* = enum 
    Program,
    Signature,
    FunctionDef,
    Code,
    IfNode,
    InfixOp,
    Operator,
    Args,
    Name,
    Int,
    Number,
    Call,
    Assign,
    Declaration,
    DeclarationHelper,
    ForRange,
    ForIn,
    ReturnNode,
    BigO,
    ComplexitySignature,
    CallArgs,
    # Good
    RightCall


  Node* = ref object
    case kind*: NodeKind:
    of Name, Operator:
      name*: string
    of Int, Number:
      i*:    int
    of DeclarationHelper:
      declaration*: DeclarationKind
    else:
      children*: seq[Node]
    typ*: Type

  DeclarationKind = enum DeclLet, DeclVar

  TypeKind* = enum
    TSimple,
    TConcrete,
    TGeneric


  Type* = object
    case kind*: TypeKind:
    of TSimple:
      discard      
    of TConcrete:
      params*:  seq[Type]
      base*:    seq[Type]
    of TGeneric:
      args*:    seq[string]
    name*:      string

macro init*(kindValue: untyped, childrenValue: untyped): untyped =
  var childrenNode = childrenValue # quote do: @[]
  # for value in childrenValue:
  #   childrenNode[1].add(value)
  result = quote:
    Node(kind: `kindValue`, children: `childrenNode`)

proc init*(kind: NodeKind, i: int): Node =
  #assert kind == Int
  case kind:
  of Int:
    Node(kind: Int, i: i)
  of Number:
    Node(kind: Number, i: i)
  else:
    nil


proc init*(kind: NodeKind, name: string): Node =
  assert kind == Name
  Node(kind: Name, name: name)

proc text*(node: Node, depth: int): string =
  if node.isNil:
    return repeat("  ", depth) & "nil"
  result = case node.kind:
    of Name, Operator:
      $node.name
    of Int:
      $node.i
    of DeclarationHelper:
      $node.declaration
    else:
      $node.kind & ":\n" & node.children.mapIt(it.text(depth + 1)).join("\n")
  result = repeat("  ", depth) & result

proc `$`*(node: Node): string =
  text(node,  0)

proc `[]`*(node: Node; index: int): Node =
  node.children[index]

proc `[]=`*(node: Node; index: int, value: Node): Node =
  node.children[index] = value
