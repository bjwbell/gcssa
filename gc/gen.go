package gc

func temp(t *Type) *Node {
	n := Nod(OXXX, nil, nil)
	Tempname(n, t)
	n.Sym.Def.Used = true
	return n.Orig
}

func Tempname(nn *Node, t *Type) {
	if t == nil {
		panic("tempname called with nil type")
	}

	// give each tmp a different name so that there
	// a chance to registerizer them
	var s *Sym = nil //Lookupf("autotmp_%.4d", statuniqgen)
	statuniqgen++
	n := Nod(ONAME, nil, nil)
	n.Sym = s
	s.Def = n
	n.Type = t
	n.Class = PAUTO
	n.Addable = true
	n.Ullman = 1
	//n.Esc = EscNever
	n.Name.Curfn = Curfn
	Curfn.Func.Dcl = list(Curfn.Func.Dcl, n)

	dowidth(t)
	n.Xoffset = 0
	*nn = *n
}
