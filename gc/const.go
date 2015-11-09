package gc

import "math/big"

func (n *Node) IntLiteral() (x int64, ok bool) {
	switch {
	case n == nil:
		return
	case Isconst(n, CTINT):
		return n.Int(), true
	case Isconst(n, CTBOOL):
		panic("unimplemented")
		//return int64(obj.Bool2int(n.Bool())), true
	}
	return
}

// Int returns n as an int.
// n must be an integer constant.
func (n *Node) Int() int64 {
	if !Isconst(n, CTINT) {
		panic("Int(v)")
	}
	return Mpgetfix(n.Val().U.(*Mpint))
}

// SetInt sets n's value to i.
// n must be an integer constant.
func (n *Node) SetInt(i int64) {
	if !Isconst(n, CTINT) {
		panic("SetInt(v)")
	}
	//Mpmovecfix(n.Val().U.(*Mpint), i)
	panic("unimplemented")
}

// SetBigInt sets n's value to x.
// n must be an integer constant.
func (n *Node) SetBigInt(x *big.Int) {
	if !Isconst(n, CTINT) {
		panic("SetBigInt(v)")
	}
	n.Val().U.(*Mpint).Val.Set(x)
}

// Bool returns n as an bool.
// n must be an boolean constant.
func (n *Node) Bool() bool {
	if !Isconst(n, CTBOOL) {
		panic("Int(v)")
	}
	return n.Val().U.(bool)
}

func consttype(n *Node) int {
	if n == nil || n.Op != OLITERAL {
		return -1
	}
	return int(n.Val().Ctype())
}

func Isconst(n *Node, ct int) bool {
	t := consttype(n)

	// If the caller is asking for CTINT, allow CTRUNE too.
	// Makes life easier for back ends.
	return t == ct || (ct == CTINT && t == CTRUNE)
}
