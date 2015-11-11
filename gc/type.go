package gc

import (
	"go/types"

	"github.com/bjwbell/ssa"
)

type Type struct {
	n *Node
	types.Type
}

var typeToEtype = map[types.BasicKind]int64{
	types.Bool:          TBOOL,
	types.Int:           TINT,
	types.Int8:          TINT8,
	types.Int16:         TINT16,
	types.Int32:         TINT32,
	types.Int64:         TINT64,
	types.Uint:          TUINT,
	types.Uint8:         TUINT8,
	types.Uint16:        TUINT16,
	types.Uint32:        TUINT32,
	types.Uint64:        TUINT64,
	types.Uintptr:       TUINTPTR,
	types.Float32:       TFLOAT32,
	types.Float64:       TFLOAT64,
	types.Complex64:     TCOMPLEX64,
	types.Complex128:    TCOMPLEX128,
	types.String:        TSTRING,
	types.UnsafePointer: TUNSAFEPTR,

	// types for untyped values
	/*types.UntypedBool:    CTBOOL,
	types.UntypedInt:     CTINT,
	types.UntypedRune:    CTRUNE,
	types.UntypedFloat:   CTFLT,
	types.UntypedComplex: CTCPLX,
	types.UntypedString:  CTSTR,
	types.UntypedNil:     CTNIL*/}

// Basic returns *types.Basic if t.Type is *types.Basic
// else nil is returned.
func (t *Type) Basic() *types.Basic {
	if basic, ok := t.Type.(*types.Basic); ok {
		return basic
	}
	return nil
}

// Struct returns *types.Struct if t.Type is *types.Struct
// else nil is returned.
func (t *Type) Struct() *types.Struct {
	if s, ok := t.Type.(*types.Struct); ok {
		return s
	}
	return nil
}

// Array returns *types.Array if t.Type is *types.Array
// else nil is returned.
func (t *Type) Array() *types.Array {
	if array, ok := t.Type.(*types.Array); ok {
		return array
	}
	return nil
}

// IsBasicInfoFlag returns true if t.Type is types.Basic and
// the BasicInfo for t.Type matches flags, otherwise false is returned
func (t *Type) IsBasicInfoFlag(flag types.BasicInfo) bool {
	if basic := t.Basic(); basic != nil {
		info := basic.Info()
		return info&flag == 1
	} else {
		return false
	}
}

func (t *Type) IsBasic() bool {
	return t.Basic() != nil
}

func (t *Type) IsBoolean() bool {
	return t.IsBasicInfoFlag(types.IsBoolean)
}

func (t *Type) IsInteger() bool {
	return t.IsBasicInfoFlag(types.IsInteger)
}

func (t *Type) IsSigned() bool {
	return (t.IsBasic() && !t.IsBasicInfoFlag(types.IsUnsigned))
}

func (t *Type) IsFloat() bool {
	return t.IsBasicInfoFlag(types.IsFloat)
}

func (t *Type) IsComplex() bool {
	return t.IsBasicInfoFlag(types.IsComplex)
}

func (t *Type) IsPtr() bool {
	// behavior should match:
	// Etype == TPTR32 || Etype == TPTR64 || Etype == TUNSAFEPTR ||
	// Etype == TMAP || Etype == TCHAN || Etype == TFUNC
	if basic := t.Basic(); basic != nil {
		return basic.Kind() == types.UnsafePointer
	}
	switch t.Type.(type) {
	case *types.Pointer, *types.Map, *types.Signature, *types.Chan:
		return true
	}
	return false
}

func (t *Type) IsString() bool {
	return t.IsBasicInfoFlag(types.IsString)
}

func (t *Type) IsMap() bool {
	_, ok := t.Type.(*types.Map)
	return ok
}

func (t *Type) IsChan() bool {
	_, ok := t.Type.(*types.Map)
	return ok
}

func (t *Type) IsSlice() bool {
	_, ok := t.Type.(*types.Slice)
	return ok
}

func (t *Type) IsArray() bool {
	_, ok := t.Type.(*types.Array)
	return ok
}

func (t *Type) IsStruct() bool {
	return t.Struct() != nil
}

func (t *Type) IsInterface() bool {
	_, ok := t.Type.(*types.Interface)
	return ok
}

func (t *Type) Size() int64 {
	var std types.StdSizes
	return std.Sizeof(t.Type)
}

func (t *Type) Alignment() int64 {
	var std types.StdSizes
	return std.Alignof(t.Type)
}

func (t *Type) IsMemory() bool { return false } // special ssa-package-only types
func (t *Type) IsFlags() bool  { return false }
func (t *Type) IsVoid() bool   { return false }

// Elem, if t.Type is []T or *T or [n]T, return T, otherwise return nil
func (t *Type) Elem() ssa.Type {
	if t.IsSlice() || t.IsPtr() || t.IsArray() {
		return &Type{t.n, t.Underlying()}
	} else {
		return nil
	}
}

// PtrTo, given T, returns *T
func (t *Type) PtrTo() ssa.Type {
	return &Type{t.n, types.NewPointer(t.Type)}
}

// NumFields returns the # of fields of a struct, panics if t is not a types.Struct
func (t *Type) NumFields() int64 {
	if !t.IsStruct() {
		panic("NumFields can only be called with Struct's")
	}
	s := t.Type.(*types.Struct)
	return int64(s.NumFields())
}

// FieldTypes returns the type of ith field of the struct and panics on error
func (t *Type) FieldType(i int64) ssa.Type {
	if s := t.Struct(); s == nil {
		panic("FieldType can only be called with Struct's")
	} else {
		if int64(s.NumFields()) <= i {
			panic("Invalid field #")
		}
		// TODO: figure out what node i.e. t.n to use
		field := Type{nil, s.Field(int(i)).Type()}
		return &field
	}
}

// FieldOff returns the offset of ith field of the struct and panics on error
func (t *Type) FieldOff(i int64) int64 {
	if s := t.Struct(); s == nil {
		panic("FieldOff can only be called with Struct's")
	} else {
		if int64(s.NumFields()) <= i {
			panic("Invalid field #")
		}
		var stdSizes types.StdSizes
		field := s.Field(int(i))
		offsets := stdSizes.Offsetsof([]*types.Var{field})
		return offsets[0]
	}
}

// NumElem returns the # of elements of an array and panics on error
func (t *Type) NumElem() int64 {
	if array := t.Array(); array == nil {
		panic("NumElem can only be called with types.Array")
	} else {
		return array.Len()
	}
}

func (t *Type) String() string {
	return t.Type.String()
}

// SimpleString is a coarser generic description of T, e.g. T's underlying type
func (t *Type) SimpleString() string {
	return t.Type.Underlying().String()
}

func (t *Type) Equal(v ssa.Type) bool {
	if v2, ok := v.(*Type); ok {
		return types.Identical(t, v2)
	}
	return false
}

// Bound returns the num elements if t is an array, if t is a slice it returns -1,
// and if t is neither an array or slice it panics
func (t *Type) Bound() int64 {
	if t.Array() != nil {
		return t.NumElem()
	} else if t.IsSlice() {
		return -1
	} else {
		panic("Bound called with invalid type")
	}
}

func (t *Type) Width() int64 {
	return t.Size()
}

// Etype is the concrete type, e.g. TBOOL, TINT64, TFIELD, etc

func (t *Type) Etype() int64 {
	if basic := t.Basic(); basic != nil {
		if etype, ok := typeToEtype[basic.Kind()]; ok {
			return etype
		} else {
			panic("unknown basic type")
		}
	}
	switch t.Type.(type) {
	case *types.Array:
		return TARRAY
	case *types.Chan:
		return TCHAN
	case *types.Interface:
		return TINTER
	case *types.Map:
		return TMAP
	case *types.Named:
		panic("unimplemented")
	case *types.Pointer:
		// HACK!
		// hardcoded to 64-bit pointers
		return TPTR64
	case *types.Signature:
		return TFUNC
	case *types.Slice:
		panic("unimplemented")
	case *types.Struct:
		return TSTRUCT
	case *types.Tuple:
		panic("unimplemented")

	}
	// TODO figure out what should be done for types.Var
	//case *types.Var:
	//	return TFIELD

	panic("unimplemented")
}

func typ(etype int) *Type {
	for t, et := range typeToEtype {
		if et == int64(etype) {
			return &Type{nil, types.Typ[t]}
		}
	}
	return nil
}

func typeinit() {
	if Widthptr == 0 {
		Fatalf("typeinit before betypeinit")
	}

	for i := 0; i < NTYPE; i++ {
		Simtype[i] = uint8(i)
	}

	Tptr = TPTR32
	if Widthptr == 8 {
		Tptr = TPTR64
	}

	for i := TINT8; i <= TUINT64; i++ {
		Isint[i] = true
	}

	Types[TINT8] = typ(TINT8)
	Types[TUINT8] = typ(TUINT8)
	Types[TINT16] = typ(TINT16)
	Types[TUINT16] = typ(TUINT16)
	Types[TINT32] = typ(TINT32)
	Types[TUINT32] = typ(TUINT32)
	Types[TINT64] = typ(TINT64)
	Types[TUINT64] = typ(TUINT64)
	Types[TINT] = typ(TINT)
	Types[TUINT] = typ(TUINT)
	Types[TUINTPTR] = typ(TUINTPTR)

	Types[TCOMPLEX64] = typ(TCOMPLEX64)
	Types[TCOMPLEX128] = typ(TCOMPLEX128)

	Types[TFLOAT32] = typ(TFLOAT32)
	Types[TFLOAT64] = typ(TFLOAT64)

	Types[TBOOL] = typ(TBOOL)

	Types[TPTR32] = typ(TPTR32)
	Types[TPTR64] = typ(TPTR64)

	Types[TFUNC] = typ(TFUNC)
	Types[TARRAY] = typ(TARRAY)
	Types[TSTRUCT] = typ(TSTRUCT)
	Types[TCHAN] = typ(TCHAN)
	Types[TMAP] = typ(TMAP)
	Types[TINTER] = typ(TINTER)
	Types[TFIELD] = typ(TFIELD)
	Types[TANY] = typ(TANY)
	Types[TSTRING] = typ(TSTRING)
	Types[TUNSAFEPTR] = typ(TUNSAFEPTR)

	// pseudo-types for literals
	Types[TIDEAL] = typ(TIDEAL)
	Types[TNIL] = typ(TNIL)
	Types[TBLANK] = typ(TBLANK)

	// pseudo-type for frame layout
	Types[TFUNCARGS] = typ(TFUNCARGS)
	Types[TCHANARGS] = typ(TCHANARGS)
	Types[TINTERMETH] = typ(TINTERMETH)

	Isint[TINT] = true
	Isint[TUINT] = true
	Isint[TUINTPTR] = true

	Isfloat[TFLOAT32] = true
	Isfloat[TFLOAT64] = true

	Iscomplex[TCOMPLEX64] = true
	Iscomplex[TCOMPLEX128] = true

	Isptr[TPTR32] = true
	Isptr[TPTR64] = true

	Issigned[TINT] = true
	Issigned[TINT8] = true
	Issigned[TINT16] = true
	Issigned[TINT32] = true
	Issigned[TINT64] = true

	/*
	 * initialize okfor
	 */
	for i := 0; i < NTYPE; i++ {
		if Isint[i] || i == TIDEAL {
			okforeq[i] = true
			okforcmp[i] = true
			okforarith[i] = true
			okforadd[i] = true
			okforand[i] = true
			okforconst[i] = true
			issimple[i] = true
			Minintval[i] = new(Mpint)
			Maxintval[i] = new(Mpint)
		}

		if Isfloat[i] {
			okforeq[i] = true
			okforcmp[i] = true
			okforadd[i] = true
			okforarith[i] = true
			okforconst[i] = true
			issimple[i] = true
			//minfltval[i] = newMpflt()
			//maxfltval[i] = newMpflt()
		}

		if Iscomplex[i] {
			okforeq[i] = true
			okforadd[i] = true
			okforarith[i] = true
			okforconst[i] = true
			issimple[i] = true
		}
	}

	issimple[TBOOL] = true

	okforadd[TSTRING] = true

	okforbool[TBOOL] = true

	okforcap[TARRAY] = true
	okforcap[TCHAN] = true

	okforconst[TBOOL] = true
	okforconst[TSTRING] = true

	okforlen[TARRAY] = true
	okforlen[TCHAN] = true
	okforlen[TMAP] = true
	okforlen[TSTRING] = true

	okforeq[TPTR32] = true
	okforeq[TPTR64] = true
	okforeq[TUNSAFEPTR] = true
	okforeq[TINTER] = true
	okforeq[TCHAN] = true
	okforeq[TSTRING] = true
	okforeq[TBOOL] = true
	okforeq[TMAP] = true    // nil only; refined in typecheck
	okforeq[TFUNC] = true   // nil only; refined in typecheck
	okforeq[TARRAY] = true  // nil slice only; refined in typecheck
	okforeq[TSTRUCT] = true // it's complicated; refined in typecheck

	okforcmp[TSTRING] = true

	var i int
	for i = 0; i < len(okfor); i++ {
		okfor[i] = okfornone[:]
	}

	// binary
	okfor[OADD] = okforadd[:]

	okfor[OAND] = okforand[:]
	okfor[OANDAND] = okforbool[:]
	okfor[OANDNOT] = okforand[:]
	okfor[ODIV] = okforarith[:]
	okfor[OEQ] = okforeq[:]
	okfor[OGE] = okforcmp[:]
	okfor[OGT] = okforcmp[:]
	okfor[OLE] = okforcmp[:]
	okfor[OLT] = okforcmp[:]
	okfor[OMOD] = okforand[:]
	okfor[OHMUL] = okforarith[:]
	okfor[OMUL] = okforarith[:]
	okfor[ONE] = okforeq[:]
	okfor[OOR] = okforand[:]
	okfor[OOROR] = okforbool[:]
	okfor[OSUB] = okforarith[:]
	okfor[OXOR] = okforand[:]
	okfor[OLSH] = okforand[:]
	okfor[ORSH] = okforand[:]

	// unary
	okfor[OCOM] = okforand[:]

	okfor[OMINUS] = okforarith[:]
	okfor[ONOT] = okforbool[:]
	okfor[OPLUS] = okforarith[:]

	// special
	okfor[OCAP] = okforcap[:]

	okfor[OLEN] = okforlen[:]

	// comparison
	iscmp[OLT] = true

	iscmp[OGT] = true
	iscmp[OGE] = true
	iscmp[OLE] = true
	iscmp[OEQ] = true
	iscmp[ONE] = true

	/*mpatofix(Maxintval[TINT8], "0x7f")
	mpatofix(Minintval[TINT8], "-0x80")
	mpatofix(Maxintval[TINT16], "0x7fff")
	mpatofix(Minintval[TINT16], "-0x8000")
	mpatofix(Maxintval[TINT32], "0x7fffffff")
	mpatofix(Minintval[TINT32], "-0x80000000")
	mpatofix(Maxintval[TINT64], "0x7fffffffffffffff")
	mpatofix(Minintval[TINT64], "-0x8000000000000000")

	mpatofix(Maxintval[TUINT8], "0xff")
	mpatofix(Maxintval[TUINT16], "0xffff")
	mpatofix(Maxintval[TUINT32], "0xffffffff")
	mpatofix(Maxintval[TUINT64], "0xffffffffffffffff")*/

	/* f is valid float if min < f < max.  (min and max are not themselves valid.) */
	//mpatoflt(maxfltval[TFLOAT32], "33554431p103") /* 2^24-1 p (127-23) + 1/2 ulp*/
	//mpatoflt(minfltval[TFLOAT32], "-33554431p103")
	//mpatoflt(maxfltval[TFLOAT64], "18014398509481983p970") /* 2^53-1 p (1023-52) + 1/2 ulp */
	//mpatoflt(minfltval[TFLOAT64], "-18014398509481983p970")

	maxfltval[TCOMPLEX64] = maxfltval[TFLOAT32]
	minfltval[TCOMPLEX64] = minfltval[TFLOAT32]
	maxfltval[TCOMPLEX128] = maxfltval[TFLOAT64]
	minfltval[TCOMPLEX128] = minfltval[TFLOAT64]

	/* simple aliases */
	Simtype[TMAP] = uint8(Tptr)

	Simtype[TCHAN] = uint8(Tptr)
	Simtype[TFUNC] = uint8(Tptr)
	Simtype[TUNSAFEPTR] = uint8(Tptr)

	/* pick up the backend thearch.typedefs */
	Array_array = int(Rnd(0, int64(Widthptr)))
	Array_nel = int(Rnd(int64(Array_array)+int64(Widthptr), int64(Widthint)))
	Array_cap = int(Rnd(int64(Array_nel)+int64(Widthint), int64(Widthint)))
	sizeof_Array = int(Rnd(int64(Array_cap)+int64(Widthint), int64(Widthptr)))

	// string is same as slice wo the cap
	sizeof_String = int(Rnd(int64(Array_nel)+int64(Widthint), int64(Widthptr)))

}
