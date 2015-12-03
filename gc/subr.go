// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"fmt"
	"go/types"
)

type Error struct {
	lineno int
	seq    int
	msg    string
}

var errors []Error

func errorexit() {
	panic("")
	/*Flusherrors()
	if outfile != "" {
		os.Remove(outfile)
	}
	os.Exit(2)*/
}

/*func parserline() int {
	if parsing && theparser.Lookahead() > 0 {
		// parser has one symbol lookahead
		return int(prevlineno)
	}
	return int(lineno)
}*/

/*func adderrorname(n *Node) {
	if n.Op != ODOT {
		return
	}
	old := fmt.Sprintf("%v: undefined: %v\n", n.Line(), n.Left)
	if len(errors) > 0 && int32(errors[len(errors)-1].lineno) == n.Lineno && errors[len(errors)-1].msg == old {
		errors[len(errors)-1].msg = fmt.Sprintf("%v: undefined: %v in %v\n", n.Line(), n.Left, n)
	}
}

func adderr(line int, format string, args ...interface{}) {
	errors = append(errors, Error{
		seq:    len(errors),
		lineno: line,
		msg:    fmt.Sprintf("%v: %s\n", Ctxt.Line(line), fmt.Sprintf(format, args...)),
	})
}*/

// errcmp sorts errors by line, then seq, then message.
type errcmp []Error

func (x errcmp) Len() int      { return len(x) }
func (x errcmp) Swap(i, j int) { x[i], x[j] = x[j], x[i] }
func (x errcmp) Less(i, j int) bool {
	a := &x[i]
	b := &x[j]
	if a.lineno != b.lineno {
		return a.lineno < b.lineno
	}
	if a.seq != b.seq {
		return a.seq < b.seq
	}
	return a.msg < b.msg
}

/*func Flusherrors() {
	bstdout.Flush()
	if len(errors) == 0 {
		return
	}
	sort.Sort(errcmp(errors))
	for i := 0; i < len(errors); i++ {
		if i == 0 || errors[i].msg != errors[i-1].msg {
			fmt.Printf("%s", errors[i].msg)
		}
	}
	errors = errors[:0]
}

func hcrash() {
	if Debug['h'] != 0 {
		Flusherrors()
		if outfile != "" {
			os.Remove(outfile)
		}
		var x *int
		*x = 0
	}
}

func yyerrorl(line int, format string, args ...interface{}) {
	adderr(line, format, args...)

	hcrash()
	nerrors++
	if nsavederrors+nerrors >= 10 && Debug['e'] == 0 {
		Flusherrors()
		fmt.Printf("%v: too many errors\n", Ctxt.Line(line))
		errorexit()
	}
}

var yyerror_lastsyntax int

func Yyerror(format string, args ...interface{}) {
	msg := fmt.Sprintf(format, args...)
	if strings.HasPrefix(msg, "syntax error") {
		nsyntaxerrors++

		// An unexpected EOF caused a syntax error. Use the previous
		// line number since getc generated a fake newline character.
		if curio.eofnl {
			lexlineno = prevlineno
		}

		// only one syntax error per line
		if int32(yyerror_lastsyntax) == lexlineno {
			return
		}
		yyerror_lastsyntax = int(lexlineno)

		// plain "syntax error" gets "near foo" added
		if msg == "syntax error" {
			yyerrorl(int(lexlineno), "syntax error near %s", lexbuf.String())
			return
		}

		// The grammar has { and LBRACE but both show up as {.
		// Rewrite syntax error referring to "{ or {" to say just "{".
		// The grammar has ? and @ but only for reading imports.
		// Silence them in ordinary errors.
		msg = strings.Replace(msg, "{ or {", "{", -1)
		msg = strings.Replace(msg, " or ?", "", -1)
		msg = strings.Replace(msg, " or @", "", -1)

		msg = strings.Replace(msg, "LLITERAL", litbuf, -1)

		yyerrorl(int(lexlineno), "%s", msg)
		return
	}

	adderr(parserline(), "%s", msg)

	hcrash()
	nerrors++
	if nsavederrors+nerrors >= 10 && Debug['e'] == 0 {
		Flusherrors()
		fmt.Printf("%v: too many errors\n", Ctxt.Line(parserline()))
		errorexit()
	}
}
*/

func Warn(fmt_ string, args ...interface{}) {
	fmt.Printf("Warning: "+fmt_, args)
}

func Warnl(line int, fmt_ string, args ...interface{}) {
	fmt.Printf("Warning (line %v): "+fmt_+" ", line, args)
}

func Fatalf(fmt_ string, args ...interface{}) {
	//Flusherrors()
	msg := "internal compiler error: " + fmt_
	fmt.Printf(msg, args)
	fmt.Printf("\n")

	// If this is a released compiler version, ask for a bug report.
	/*if strings.HasPrefix(obj.Getgoversion(), "release") {
		fmt.Printf("\n")
		fmt.Printf("Please file a bug report including a short program that triggers the error.\n")
		fmt.Printf("https://golang.org/issue/new\n")
	}*/

	//hcrash()
	errorexit()
}

/*func linehistpragma(file string) {
	if Debug['i'] != 0 {
		fmt.Printf("pragma %s at line %v\n", file, Ctxt.Line(int(lexlineno)))
	}
	Ctxt.AddImport(file)
}

func linehistpush(file string) {
	if Debug['i'] != 0 {
		fmt.Printf("import %s at line %v\n", file, Ctxt.Line(int(lexlineno)))
	}
	Ctxt.LineHist.Push(int(lexlineno), file)
}

func linehistpop() {
	if Debug['i'] != 0 {
		fmt.Printf("end of import at line %v\n", Ctxt.Line(int(lexlineno)))
	}
	Ctxt.LineHist.Pop(int(lexlineno))
}

func linehistupdate(file string, off int) {
	if Debug['i'] != 0 {
		fmt.Printf("line %s at line %v\n", file, Ctxt.Line(int(lexlineno)))
	}
	Ctxt.LineHist.Update(int(lexlineno), file, off)
}*/

func setlineno(n *Node) int32 {
	// TODO
	return 0
	/*lno := lineno
	if n != nil {
		switch n.Op {
		case ONAME, OTYPE, OPACK:
			break

		case OLITERAL:
			if n.Sym != nil {
				break
			}
			fallthrough

		default:
			lineno = n.Lineno
			if lineno == 0 {
				if Debug['K'] != 0 {
					Warn("setlineno: line 0")
				}
				lineno = lno
			}
		}
	}

	return lno*/
}

func Lookup(name string) *Sym {
	return nil
	//return localpkg.Lookup(name)
}

func Lookupf(format string, a ...interface{}) *Sym {
	return Lookup(fmt.Sprintf(format, a...))
}

func LookupBytes(name []byte) *Sym {
	return nil
	//return localpkg.LookupBytes(name)
}

var initSyms []*Sym

func Nod(op int, nleft *Node, nright *Node) *Node {
	return nil
	/*n := new(Node)
	n.Op = uint8(op)
	n.Left = nleft
	n.Right = nright
	n.Lineno = int32(parserline())
	n.Xoffset = BADWIDTH
	n.Orig = n
	switch op {
	case OCLOSURE, ODCLFUNC:
		n.Func = new(Func)
		n.Func.FCurfn = Curfn
	case ONAME:
		n.Name = new(Name)
		n.Name.Param = new(Param)
	case OLABEL, OPACK:
		n.Name = new(Name)
	case ODCLFIELD:
		if nleft != nil {
			n.Name = nleft.Name
		} else {
			n.Name = new(Name)
			n.Name.Param = new(Param)
		}
	}
	if n.Name != nil {
		n.Name.Curfn = Curfn
	}
	return n*/
}

func saveorignode(n *Node) {
	return
	/*if n.Orig != nil {
		return
	}
	norig := Nod(int(n.Op), nil, nil)
	*norig = *n
	n.Orig = norig*/
}

// ispaddedfield reports whether the given field
// is followed by padding. For the case where t is
// the last field, total gives the size of the enclosing struct.
func ispaddedfield(t *Type, total int64) bool {
	return false
	/*if t.Etype() != TFIELD {
		Fatalf("ispaddedfield called non-field %v", t)
	}
	if t.Down == nil {
		return t.Width+t.Type.Width != total
	}
	return t.Width+t.Type.Width != t.Down.Width*/
}

/*func typ(et int) *Type {
	t := new(Type)
	t.Etype() = uint8(et)
	t.Width = BADWIDTH
	t.Lineno = int(lineno)
	t.Orig = t
	return t
}*/

func Nodintconst(v int64) *Node {
	return nil
	/*c := Nod(OLITERAL, nil, nil)
	c.Addable = true
	c.SetVal(Val{new(Mpint)})
	Mpmovecfix(c.Val().U.(*Mpint), v)
	c.Type = Types[TIDEAL]
	return c*/
}

func nodfltconst(v *Mpflt) *Node {
	return nil
	/*c := Nod(OLITERAL, nil, nil)
	c.Addable = true
	c.SetVal(Val{newMpflt()})
	mpmovefltflt(c.Val().U.(*Mpflt), v)
	c.Type = Types[TIDEAL]
	return c*/
}

func Nodconst(n *Node, t *Type, v int64) {
	return
	/**n = Node{}
	n.Op = OLITERAL
	n.Addable = true
	n.SetVal(Val{new(Mpint)})
	Mpmovecfix(n.Val().U.(*Mpint), v)
	n.Type = t

	if Isfloat[t.Etype()] {
		Fatalf("nodconst: bad type %v", t)
	}*/
}

func nodnil() *Node {
	return nil
	/*c := Nodintconst(0)
	c.SetVal(Val{new(NilVal)})
	c.Type = Types[TNIL]
	return c*/
}

func Nodbool(b bool) *Node {
	return nil
	/*c := Nodintconst(0)
	c.SetVal(Val{b})
	c.Type = idealbool
	return c*/
}

func aindex(b *Node, t *Type) *Type {
	return nil
	/*bound := int64(-1) // open bound
	typecheck(&b, Erv)
	if b != nil {
		switch consttype(b) {
		default:
			Yyerror("array bound must be an integer expression")

		case CTINT, CTRUNE:
			bound = Mpgetfix(b.Val().U.(*Mpint))
			if bound < 0 {
				Yyerror("array bound must be non negative")
			}
		}
	}

	// fixed array
	r := typ(TARRAY)

	r.Type = t
	r.Bound = bound
	return r*/
}

func isnil(n *Node) bool {
	if n == nil {
		return false
	}
	if n.Op() != OLITERAL {
		return false
	}
	if n.Val().Ctype() != CTNIL {
		return false
	}
	return true
}

func isptrto(t *Type, et int) bool {
	if t == nil {
		return false
	}
	if !Isptr[t.Etype()] {
		return false
	}
	t = t.Elem().(*Type)
	if t == nil {
		return false
	}
	if int(t.Etype()) != et {
		return false
	}
	return true
}

func Istype(t *Type, et int) bool {
	return t != nil && int(t.Etype()) == et
}

func Isfixedarray(t *Type) bool {
	return t != nil && t.Etype() == TARRAY && t.Bound() >= 0
}

func Isslice(t *Type) bool {
	return t != nil && t.Etype() == TARRAY && t.Bound() < 0
}

func isblank(n *Node) bool {
	return false
	/*if n == nil {
		return false
	}
	return isblanksym(n.Sym)*/
}

func isblanksym(s *Symbol) bool {
	return s != nil && s.Name() == "_"
}

func Isinter(t *Type) bool {
	return t != nil && t.Etype() == TINTER
}

func isnilinter(t *Type) bool {
	if !Isinter(t) {
		return false
	}
	if t.Type != nil {
		return false
	}
	return true
}

func isideal(t *Type) bool {
	if t == nil {
		return false
	}
	if t == idealstring || t == idealbool {
		return true
	}
	switch t.Etype() {
	case TNIL, TIDEAL:
		return true
	}

	return false
}

func cplxsubtype(et int) int {
	switch et {
	case TCOMPLEX64:
		return TFLOAT32

	case TCOMPLEX128:
		return TFLOAT64
	}

	Fatalf("cplxsubtype: %v\n", Econv(int(et), 0))
	return 0
}

func eqnote(a, b *string) bool {
	return a == b || a != nil && b != nil && *a == *b
}

type TypePairList struct {
	t1   *Type
	t2   *Type
	next *TypePairList
}

func onlist(l *TypePairList, t1 *Type, t2 *Type) bool {
	for ; l != nil; l = l.next {
		if (l.t1 == t1 && l.t2 == t2) || (l.t1 == t2 && l.t2 == t1) {
			return true
		}
	}
	return false
}

// Return 1 if t1 and t2 are identical, following the spec rules.
//
// Any cyclic type must go through a named type, and if one is
// named, it is only identical to the other if they are the same
// pointer (t1 == t2), so there's no chance of chasing cycles
// ad infinitum, so no need for a depth counter.
func Eqtype(t1 *Type, t2 *Type) bool {
	return types.Identical(t1.Type, t2.Type)
}

// Is type src assignment compatible to type dst?
// If so, return op code to use in conversion.
// If not, return 0.
func assignop(src *Type, dst *Type, why *string) NodeOp {
	if !types.AssignableTo(src.Type, dst.Type) {
		return 0
	}

	if src == dst {
		return OCONVNOP
	}
	if src == nil || dst == nil {
		return 0
	}

	// 1. src type is identical to dst.
	if Eqtype(src, dst) {
		return OCONVNOP
	}

	// 3. dst is an interface type and src implements dst.
	if dst.IsInterface() {
		dstInterface := dst.Type.(*types.Interface)
		if types.Implements(src.Type, dstInterface) {
			return OCONVIFACE
		}
		return 0
	}

	if isptrto(dst, TINTER) {
		if why != nil {
			*why = fmt.Sprintf(":\n\t%v is pointer to interface, not interface", dst)
		}
		return 0
	}

	if src.IsChan() || dst.IsChan() {
		panic("channels not supported")
	}

	// 5. src is the predeclared identifier nil and dst is a nillable type.
	if src.Etype() == TNIL {
		switch dst.Etype() {
		case TARRAY:
			if dst.Bound() != -100 { // not slice
				break
			}
			fallthrough

		case TPTR32,
			TPTR64,
			TFUNC,
			TMAP,
			TCHAN,
			TINTER:
			return OCONVNOP
		}
	}

	// 6. rule about untyped constants - already converted by defaultlit.

	// 7. Any typed value can be assigned to the blank identifier.
	if dst.Etype() == TBLANK {
		return OCONVNOP
	}

	return 0

}

// Can we convert a value of type src to a value of type dst?
// If so, return op code to use in conversion (maybe OCONVNOP).
// If not, return 0.
func convertop(src *Type, dst *Type, why *string) NodeOp {
	if why != nil {
		*why = ""
	}

	if src == dst {
		return OCONVNOP
	}
	if src == nil || dst == nil {
		return 0
	}

	// 1. src can be assigned to dst.
	op := assignop(src, dst, why)
	if op != 0 {
		return op
	}

	// The rules for interfaces are no different in conversions
	// than assignments.  If interfaces are involved, stop now
	// with the good message from assignop.
	// Otherwise clear the error.
	if src.Etype() == TINTER || dst.Etype() == TINTER {
		return 0
	}
	if why != nil {
		*why = ""
	}

	// 2. src and dst have identical underlying types.
	if Eqtype(src, dst) {
		return OCONVNOP
	}

	// 3. src and dst are unnamed pointer types
	// and their base types have identical underlying types.
	if src.IsPtr() && dst.IsPtr() {
		if Eqtype(src.Elem().(*Type), dst.Elem().(*Type)) {
			return OCONVNOP
		}
	}

	// 4. src and dst are both integer or floating point types.
	if (Isint[src.Etype()] || Isfloat[src.Etype()]) && (Isint[dst.Etype()] || Isfloat[dst.Etype()]) {
		if Simtype[src.Etype()] == Simtype[dst.Etype()] {
			return OCONVNOP
		}
		return OCONV
	}

	// 5. src and dst are both complex types.
	if Iscomplex[src.Etype()] && Iscomplex[dst.Etype()] {
		if Simtype[src.Etype()] == Simtype[dst.Etype()] {
			return OCONVNOP
		}
		return OCONV
	}

	// 6. src is an integer or has type []byte or []rune
	// and dst is a string type.
	if Isint[src.Etype()] && dst.Etype() == TSTRING {
		return ORUNESTR
	}

	if Isslice(src) && dst.Etype() == TSTRING {
		if src.Elem().(*Type).Etype() == bytetype.Etype() {
			return OARRAYBYTESTR
		}
		if src.Elem().(*Type).Etype() == runetype.Etype() {
			return OARRAYRUNESTR
		}
	}

	// 7. src is a string and dst is []byte or []rune.
	// String to slice.
	if src.Etype() == TSTRING && Isslice(dst) {
		if dst.Elem().(*Type).Etype() == bytetype.Etype() {
			return OSTRARRAYBYTE
		}
		if dst.Elem().(*Type).Etype() == runetype.Etype() {
			return OSTRARRAYRUNE
		}
	}

	// 8. src is a pointer or uintptr and dst is unsafe.Pointer.
	if (Isptr[src.Etype()] || src.Etype() == TUINTPTR) && dst.Etype() == TUNSAFEPTR {
		return OCONVNOP
	}

	// 9. src is unsafe.Pointer and dst is a pointer or uintptr.
	if src.Etype() == TUNSAFEPTR && (Isptr[dst.Etype()] || dst.Etype() == TUINTPTR) {
		return OCONVNOP
	}

	return 0
}

func assignconv(n *Node, t *Type, context string) *Node {
	return assignconvfn(n, t, func() string { return context })
}

// Convert node n for assignment to type t.
func assignconvfn(n *Node, t *Type, context func() string) *Node {
	return nil
	/*if n == nil || n.Type == nil || n.Type.Broke {
		return n
	}

	if t.Etype() == TBLANK && n.Type.Etype() == TNIL {
		panic("use of untyped nil")
	}

	old := n
	old.Diag++ // silence errors about n; we'll issue one below
	defaultlit(&n, t)
	old.Diag--
	if t.Etype() == TBLANK {
		return n
	}

	// Convert ideal bool from comparison to plain bool
	// if the next step is non-bool (like interface{}).
	if n.Type == idealbool && t.Etype() != TBOOL {
		if n.Op == ONAME || n.Op == OLITERAL {
			r := Nod(OCONVNOP, n, nil)
			r.Type = Types[TBOOL]
			r.Typecheck = 1
			r.Implicit = true
			n = r
		}
	}

	if Eqtype(n.Type, t) {
		return n
	}

	var why string
	op := assignop(n.Type, t, &why)
	if op == 0 {
		//Yyerror("cannot use %v as type %v in %s%s", Nconv(n, obj.FmtLong), t, context(), why)
		op = OCONV
	}

	r := Nod(op, n, nil)
	r.Type = t
	r.Typecheck = 1
	r.Implicit = true
	r.Orig = n.Orig
	return r*/
}

/*
 * Is this a 64-bit type?
 */
func Is64(t *Type) bool {
	if t == nil {
		return false
	}
	switch Simtype[t.Etype()] {
	case TINT64, TUINT64, TPTR64:
		return true
	}

	return false
}

/*
 * Is a conversion between t1 and t2 a no-op?
 */
func Noconv(t1 *Type, t2 *Type) bool {
	e1 := int(Simtype[t1.Etype()])
	e2 := int(Simtype[t2.Etype()])

	switch e1 {
	case TINT8, TUINT8:
		return e2 == TINT8 || e2 == TUINT8

	case TINT16, TUINT16:
		return e2 == TINT16 || e2 == TUINT16

	case TINT32, TUINT32, TPTR32:
		return e2 == TINT32 || e2 == TUINT32 || e2 == TPTR32

	case TINT64, TUINT64, TPTR64:
		return e2 == TINT64 || e2 == TUINT64 || e2 == TPTR64

	case TFLOAT32:
		return e2 == TFLOAT32

	case TFLOAT64:
		return e2 == TFLOAT64
	}

	return false
}

/*
 * compute a hash value for type t.
 * if t is a method type, ignore the receiver
 * so that the hash can be used in interface checks.
 * %T already contains
 * all the necessary logic to generate a representation
 * of the type that completely describes it.
 * using smprint here avoids duplicating that code.
 * using md5 here is overkill, but i got tired of
 * accidental collisions making the runtime think
 * two types are equal when they really aren't.
 */
/*func typehash(t  *Type) uint32 {
	var p string

	if t.Thistuple != 0 {
		// hide method receiver from Tpretty
		t.Thistuple = 0

		p = Tconv(t, obj.FmtLeft|obj.FmtUnsigned)
		t.Thistuple = 1
	} else {
		p = Tconv(t, obj.FmtLeft|obj.FmtUnsigned)
	}

	//print("typehash: %s\n", p);
	h := md5.Sum([]byte(p))
	return binary.LittleEndian.Uint32(h[:4])
}*/

// Ptrto returns the Type *t.
// The returned struct must not be modified.
func Ptrto(t *Type) *Type {
	ptr := types.NewPointer(t.Type)
	ptrType := Type{n: t.n, Type: ptr}
	return &ptrType
}

/*
 * iterator to walk a structure declaration
 */

func getoutargx(t *Type) *Type {
	return nil
	//return *Getoutarg(t)
}

func getinargx(t *Type) *Type {
	return nil
	//return *getinarg(t)
}

func checknil(x *Node, init **NodeList) {
	//if x.Type().IsInterface() {
	//	x = Nod(OITAB, x, nil)
	//typecheck(&x, Erv)
	//}

	//n := Nod(OCHECKNIL, x, nil)
	//n.Typecheck = 1
	//*init = list(*init, n)
}
