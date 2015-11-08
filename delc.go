package gcssa

func typenod(t *Type) *Node {
	// if we copied another type with *t = *u
	// then t->nod might be out of date, so
	// check t->nod->type too
	if t.Nod == nil || t.Nod.Type != t {
		t.Nod = Nod(OTYPE, nil, nil)
		t.Nod.Type = t
		t.Nod.Sym = t.Sym
	}

	return t.Nod
}

/*
 * this generates a new name node,
 * typically for labels or other one-off names.
 */
func newname(s *Sym) *Node {
	if s == nil {
		Fatalf("newname nil")
	}

	n := Nod(ONAME, nil, nil)
	n.Sym = s
	n.Type = nil
	n.Addable = true
	n.Ullman = 1
	n.Xoffset = 0
	return n
}
