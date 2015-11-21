package gc

import (
	"fmt"
	"go/ast"
	"go/constant"
	"go/token"
	"go/types"
	"math/big"

	"github.com/bjwbell/ssa"
)

type Symbol struct {
	types.Object
}

type Node struct {
	ast.Node
	Ctx   Ctx
	class uint8
}

type IdentDef ast.Object

func (obj *IdentDef) Heapaddr() *Node {
	return nil
}

func (sym *Symbol) String() string {
	return "<Sym.String()>"
}

func (n *Node) Lineno() int32 {
	return int32(n.Ctx.file.Line(n.Pos()))
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

// For if stmts, Likely==-1 means not enough info to determine which branch
// is more likely.
func (n *Node) Likely() int8 {
	return -1
}

func (n *Node) Typ() ssa.Type {
	return n.Type()

}

func (n *Node) Val() Val {
	var expr ast.Expr
	var ok bool
	if expr, ok = n.Node.(ast.Expr); ok {
		var typeAndValue types.TypeAndValue
		if typeAndValue, ok = n.Ctx.fn.Types[expr]; ok {
			if typeAndValue.Value == nil {
				panic("internal compiler error")
			}
			if typeAndValue.Value.Kind() != constant.Int {
				panic("unimplemented")
			}
			if i64, exact := constant.Int64Val(typeAndValue.Value); exact {
				mpInt := Mpint{}
				mpInt.Val = *big.NewInt(i64)
				val := Val{}
				val.U = &mpInt
				return val
			} else {
				panic("internal compiler error")
			}
		} else {
			panic("internal compiler error")
		}
	} else if _, ok = n.Node.(*ast.BasicLit); ok {
		panic("unimplemented")

	} else {
		// TODO: other ast.*
		return Val{}
	}
}

func (n *Node) Op() NodeOp {
	switch node := n.Node.(type) {
	case *ast.ArrayType:
		return OTARRAY
	case *ast.AssignStmt:
		return OAS
	case *ast.BasicLit:
		return OLITERAL
	case *ast.BinaryExpr:
		switch node.Op {
		case token.ADD:
			return OADD
		case token.SUB:
			return OSUB
		case token.MUL:
			return OMUL
		case token.QUO:
			return ODIV
		case token.REM:
			return OMOD
		case token.AND:
			return OAND
		case token.OR:
			return OOR
		case token.XOR:
			return OXOR
		case token.SHL:
			return OLSH
		case token.SHR:
			return ORSH
		case token.AND_NOT:
			return OANDNOT
		case token.ADD_ASSIGN, token.SUB_ASSIGN, token.MUL_ASSIGN, token.QUO_ASSIGN, token.REM_ASSIGN, token.AND_ASSIGN, token.OR_ASSIGN, token.XOR_ASSIGN, token.SHL_ASSIGN, token.SHR_ASSIGN, token.AND_NOT_ASSIGN:
			return OASOP
		case token.LAND:
			return OANDAND
		case token.LOR:
			return OOROR
		case token.INC:
			return OINC
		case token.DEC:
			return ODEC
		case token.EQL:
			return OEQ
		case token.LSS:
			return OLT
		case token.GTR:
			return OGT
		case token.ASSIGN:
			return OAS
		case token.NEQ:
			return ONE
		case token.LEQ:
			return OLE
		case token.GEQ:
			return OGE
		case token.DEFINE:
			return OAS
		default:
			panic("Unknown op for binary expression")

		}
	case *ast.BlockStmt:
		return OBLOCK
	case *ast.BranchStmt:
		switch node.Tok {
		case token.BREAK:
			return OBREAK
		case token.CONTINUE:
			return OCONTINUE
		case token.GOTO:
			return OGOTO
		case token.FALLTHROUGH:
			return OFALL
		default:
			panic("Unknown token for branch statement")
		}
	case *ast.CallExpr:
		return OCALLFUNC
	case *ast.CaseClause:
		return OCASE
	case *ast.ChanType:
		panic("channels are not supported")
	case *ast.CommClause:
		panic("select statements are not supported")
	case *ast.CompositeLit:
		panic("Composite literals are unimplemented")
	case *ast.DeclStmt:
		dcl := node.Decl.(*ast.GenDecl)
		if dcl.Tok == token.VAR {
			return ODCL
		} else {
			panic("unimplemented")
		}

	case *ast.DeferStmt:
		panic("defer is unsupported")
	case *ast.Ellipsis:
		panic("unimplemented")
	case *ast.EmptyStmt:
		return OEMPTY
	case *ast.ExprStmt:
		panic("unimplemented")
	case *ast.ReturnStmt:
		return ORETURN
	case *ast.IfStmt:
		return OIF
	case *ast.Ident:
		// TODO: return OPARAM for parameters?
		return ONAME
	default:
		fmt.Printf("node: %#v\n", node)
		panic("unimplemented")
	}

}

// OREGISTER, OINDREG
func (n *Node) Reg() NodeOp {
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

func (n *Node) LeftRight() (left, right *Node) {
	switch node := n.Node.(type) {
	case *ast.BinaryExpr:
		return n.BinaryExpr()
	case *ast.DeclStmt:
		dcl := node.Decl.(*ast.GenDecl)
		if dcl.Tok == token.VAR {
			if len(dcl.Specs) != 1 {
				panic("unimplemented")
			}
			valueSpec := dcl.Specs[0].(*ast.ValueSpec)
			if len(valueSpec.Names) != 1 {
				panic("unimplemented")
			}
			// TODO
			return nil, nil
		} else {
			panic("unimplemented")
		}
	}
	return nil, nil
}

// Bounded returns true if bounds checks are unnecessary.
func (n *Node) Bounded() bool {
	// TODO, bounds checks are necessary for slices
	return n.Type().IsSlice()
}

// Addrtaken, address taken, even if not moved to heap
func (n *Node) Addrtaken() bool {
	// TODO implement
	return false
}

// Class, PPARAM, PAUTO, PEXTERN, etc
func (n *Node) Class() uint8 {
	return n.class
}
