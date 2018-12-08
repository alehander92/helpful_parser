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
    Typename,
    Int,
    Number,
    Call,
    Assign,
    Declaration,
    ForRange,
    ForIn,
    ReturnNode,
    BigO,
    ComplexitySignature,
    CallArgs,
    # Good
    RightCall,
    Index,
    BigM,
    ComplexityInfix,
    DeclarationName,
    RightInfix


  Node* = ref object
    case kind*: NodeKind:
    of Name, Operator, Typename, DeclarationName:
      name*: string
    of Int, Number:
      i*:    int
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
  if kind == Name:
    Node(kind: Name, name: name)
  elif kind == Typename:
    Node(kind: Typename, name: name)
  elif kind == DeclarationName:
    Node(kind: DeclarationName, name: name)
  else:
    nil

proc text*(node: Node, depth: int): string =
  if node.isNil:
    return repeat("  ", depth) & "nil"
  result = case node.kind:
    of Name, Operator, Typename, DeclarationName:
      $node.name
    of Int, Number:
      $node.i
    else:
      $node.kind & ":\n" & node.children.mapIt(it.text(depth + 1)).join("\n")
  result = repeat("  ", depth) & result

proc `$`*(node: Node): string =
  text(node,  0)

proc `[]`*(node: Node; index: int): Node =
  node.children[index]

proc `[]=`*(node: Node; index: int, value: Node): Node =
  node.children[index] = value
