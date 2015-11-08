package gcssa

func Rnd(o int64, r int64) int64 {
	if r < 1 || r > 8 || r&(r-1) != 0 {
		panic("rnd d")
	}
	return (o + r - 1) &^ (r - 1)
}

func dowidth(t *Type) {

	// HACK
	Widthptr = 8

	if Widthptr == 0 {
		panic("dowidth without betypeinit")
	}

	if t == nil {
		return
	}

	if t.Width > 0 {
		if t.Align == 0 {
			// See issue 11354
			panic("zero alignment with nonzero size v")
		}
		return
	}

	if t.Width == -2 {
		lno := int(lineno)
		lineno = int32(t.Lineno)
		if !t.Broke {
			t.Broke = true
			panic("invalid recursive type v") //, t)
		}

		t.Width = 0
		lineno = int32(lno)
		return
	}

	// break infinite recursion if the broken recursive type
	// is referenced again
	if t.Broke && t.Width == 0 {
		return
	}

	// defer checkwidth calls until after we're done
	//defercalc++

	lno := int(lineno)
	lineno = int32(t.Lineno)
	t.Width = -2
	t.Align = 0

	et := int32(t.Etype)
	switch et {
	case TFUNC, TCHAN, TMAP, TSTRING:
		break

		/* simtype == 0 during bootstrap */
	default:
		if Simtype[t.Etype] != 0 {
			et = int32(Simtype[t.Etype])
		}
	}

	w := int64(0)
	switch et {
	default:
		Fatalf("dowidth: unknown type: %v", t)

		/* compiler-specific stuff */
	case TINT8, TUINT8, TBOOL:
		// bool is int8
		w = 1

	case TINT16, TUINT16:
		w = 2

	case TINT32, TUINT32, TFLOAT32:
		w = 4

	case TINT64, TUINT64, TFLOAT64, TCOMPLEX64:
		w = 8
		t.Align = uint8(Widthreg)

	case TCOMPLEX128:
		w = 16
		t.Align = uint8(Widthreg)

	case TPTR32:
		w = 4
		//checkwidth(t.Type)

	case TPTR64:
		w = 8
		//checkwidth(t.Type)

	case TUNSAFEPTR:
		w = int64(Widthptr)

	case TINTER: // implemented as 2 pointers
		w = 2 * int64(Widthptr)

		t.Align = uint8(Widthptr)
		//offmod(t)

	case TCHAN: // implemented as pointer
		w = int64(Widthptr)

		//checkwidth(t.Type)

		// make fake type to check later to
		// trigger channel argument check.
		t1 := typ(TCHANARGS)

		t1.Type = t
		//checkwidth(t1)

	case TCHANARGS:
		t1 := t.Type
		dowidth(t.Type) // just in case
		if t1.Type.Width >= 1<<16 {
			panic("channel element type too large (>64kB)")
		}
		t.Width = 1

	case TMAP: // implemented as pointer
		w = int64(Widthptr)

		//checkwidth(t.Type)
		//checkwidth(t.Down)

	case TFORW: // should have been filled in
		if !t.Broke {
			panic("invalid recursive type v") //, t)
		}
		w = 1 // anything will do

		// dummy type; should be replaced before use.
	case TANY:
		if Debug['A'] == 0 {
			Fatalf("dowidth any")
		}
		w = 1 // anything will do

	case TSTRING:
		if sizeof_String == 0 {
			Fatalf("early dowidth string")
		}
		w = int64(sizeof_String)
		t.Align = uint8(Widthptr)

	case TARRAY:
		if t.Type == nil {
			break
		}
		if t.Bound >= 0 {
			dowidth(t.Type)
			if t.Type.Width != 0 {
				cap := (uint64(Thearch.MAXWIDTH) - 1) / uint64(t.Type.Width)
				if uint64(t.Bound) > cap {
					panic("type %v larger than address space")
					//Yyerror("type %v larger than address space", Tconv(t, obj.FmtLong))
				}
			}

			w = t.Bound * t.Type.Width
			t.Align = t.Type.Align
		} else if t.Bound == -1 {
			w = int64(sizeof_Array)
			//checkwidth(t.Type)
			t.Align = uint8(Widthptr)
		} else if t.Bound == -100 {
			if !t.Broke {
				panic("use of [...] array outside of array literal")
				t.Broke = true
			}
		} else {
			Fatalf("dowidth %v", t) // probably [...]T
		}

	case TSTRUCT:
		if t.Funarg {
			Fatalf("dowidth fn struct %v", t)
		}
		//w = widstruct(t, t, 0, 1)

		// make fake type to check later to
	// trigger function argument computation.
	case TFUNC:
		t1 := typ(TFUNCARGS)

		t1.Type = t
		//checkwidth(t1)

		// width of func type is pointer
		w = int64(Widthptr)

		// function is 3 cated structures;
	// compute their widths as side-effect.
	case TFUNCARGS:
		t1 := t.Type

		//w = widstruct(t.Type, *getthis(t1), 0, 0)
		//w = widstruct(t.Type, *getinarg(t1), w, Widthreg)
		//w = widstruct(t.Type, *Getoutarg(t1), w, Widthreg)
		t1.Argwid = w
		if w%int64(Widthreg) != 0 {
			panic("bad type")
			//Warn("bad type %v %d\n", t1, w)
		}
		t.Align = 1
	}

	if Widthptr == 4 && w != int64(int32(w)) {
		panic("type v too large")
	}

	t.Width = w
	if t.Align == 0 {
		if w > 8 || w&(w-1) != 0 {
			Fatalf("invalid alignment for %v", t)
		}
		t.Align = uint8(w)
	}

	lineno = int32(lno)

	//if defercalc == 1 {
	//		resumecheckwidth()
	//} else {
	//	defercalc--
	//}
}
