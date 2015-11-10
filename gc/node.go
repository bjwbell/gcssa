package gc

import (
	"go/ast"
	"go/types"

	"ssa"
)

type Symbol struct {
	types.Object
}

type Node struct {
	ast.Node
	Ctx Ctx
}

type IdentDef ast.Object

func (obj *IdentDef) Heapaddr() *Node {
	return nil
}

func (sym *Symbol) String() string {
	return "<Sym.String()>"
}

func (n *Node) Symbol() *Symbol {
	if n == nil {
		return nil
	}
	switch node := n.Node.(type) {
	case *ast.Ident:
		return &Symbol{n.Ctx.fn.ObjectOf(node)}
	default:
		return nil
	}
}

func (n *Node) Type() *Type {
	if expr, ok := n.Node.(ast.Expr); ok {
		typ := n.Ctx.fn.TypeOf(expr)
		return &Type{n: n, Type: typ}
	}
	if ident, ok := n.Node.(*ast.Ident); ok {
		obj := n.Ctx.fn.Uses[ident]
		return &Type{n: n, Type: obj.Type()}
	} else {
		panic("no type")
	}

}

func (n *Node) Typ() ssa.Type {
	return n.Type()

}

func (n *Node) Val() Val {
	return Val{}
}

func (n *Node) Op() int {
	return 0
}

// OREGISTER, OINDREG
func (n *Node) Reg() int16 {
	return OREGISTER
}

// ONAME (var, const, func def)
func (n *Node) Name() *IdentDef {
	if ident, ok := n.Node.(*ast.Ident); ok {
		obj := IdentDef(*ident.Obj)
		return &obj
	} else {
		panic("node not an identifier")
	}
}

// offset from SP for local vars
func (n *Node) Xoffset() int64 {
	return 0
}

func (n *Node) BinaryExpr() (left, right *Node) {
	if expr, ok := n.Node.(*ast.BinaryExpr); ok {
		left = &Node{Node: expr.X, Ctx: n.Ctx}
		right = &Node{Node: expr.Y, Ctx: n.Ctx}
		return

	} else {
		panic("Not a binary expr")
	}
}

func (n *Node) Left() (left *Node) {
	left, _ = n.BinaryExpr()
	return
}

func (n *Node) Right() (right *Node) {
	_, right = n.BinaryExpr()
	return
}

// Bounded returns true if bounds checks are unnecessary.
func (n *Node) Bounded() bool {
	// TODO, bounds checks are necessary for slices
	return true
}

// Addrtaken, address taken, even if not moved to heap
func (n *Node) Addrtaken() bool {
	// TODO implement
	return false
}

// Class, PPARAM, PAUTO, PEXTERN, etc
func (n *Node) Class() uint8 {
	// TODO implement
	return PPARAM
}
