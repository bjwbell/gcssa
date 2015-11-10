package gc

import (
	"go/types"

	"github.com/bjwbell/ssa"
)

type Type struct {
	n *Node
	types.Type
}

func (t *Type) IsBoolean() bool {
	return t.Etype() == TBOOL
}

func (t *Type) IsInteger() bool {
	switch t.Etype() {
	case TINT8, TUINT8, TINT16, TUINT16, TINT32, TUINT32, TINT64, TUINT64, TINT, TUINT, TUINTPTR:
		return true
	}
	return false
}

func (t *Type) IsSigned() bool {
	switch t.Etype() {
	case TINT8, TINT16, TINT32, TINT64, TINT:
		return true
	}
	return false
}

func (t *Type) IsFloat() bool {
	return t.Etype() == TFLOAT32 || t.Etype() == TFLOAT64
}

func (t *Type) IsComplex() bool {
	return t.Etype() == TCOMPLEX64 || t.Etype() == TCOMPLEX128
}

func (t *Type) IsPtr() bool {
	return t.Etype() == TPTR32 || t.Etype() == TPTR64 || t.Etype() == TUNSAFEPTR ||
		t.Etype() == TMAP || t.Etype() == TCHAN || t.Etype() == TFUNC
}

func (t *Type) IsString() bool {
	return t.Etype() == TSTRING
}

func (t *Type) IsMap() bool {
	return t.Etype() == TMAP
}

func (t *Type) IsChan() bool {
	return t.Etype() == TCHAN
}

func (t *Type) IsSlice() bool {
	return t.Etype() == TARRAY && t.Bound() < 0
}

func (t *Type) IsArray() bool {
	return t.Etype() == TARRAY && t.Bound() >= 0
}

func (t *Type) IsStruct() bool {
	return t.Etype() == TSTRUCT
}

func (t *Type) IsInterface() bool {
	return types.IsInterface(t.Type)
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

func (t *Type) Elem() ssa.Type  { return nil } // given []T or *T or [n]T, return T
func (t *Type) PtrTo() ssa.Type { return nil } // given T, return *T

func (t *Type) NumFields() int64           { return 0 }   // # of fields of a struct
func (t *Type) FieldType(i int64) ssa.Type { return nil } // type of ith field of the struct
func (t *Type) FieldOff(i int64) int64     { return 0 }   // offset of ith field of the struct

func (t *Type) NumElem() int64 { return 0 } // # of elements of an array

func (t *Type) String() string       { return "<Type.String()>" }
func (t *Type) SimpleString() string { return "<Type.String()>" } // a coarser generic description of T, e.g. T's underlying type
func (t *Type) Equal(ssa.Type) bool  { return false }

// TARRAY, negative if slice
func (t *Type) Bound() int64 {
	return -1
}

func (t *Type) Width() int64 {
	return t.Size()
}

// Etype is the concrete type, e.g. TBOOL, TINT64, TFIELD, etc
func (t *Type) Etype() int64 {
	// TODO
	return 0
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
	Isint[TINT] = true
	Isint[TUINT] = true
	Isint[TUINTPTR] = true

	Isfloat[TFLOAT32] = true
	Isfloat[TFLOAT64] = true

	Iscomplex[TCOMPLEX64] = true
	Iscomplex[TCOMPLEX128] = true

	Isptr[TPTR32] = true
	Isptr[TPTR64] = true

	isforw[TFORW] = true

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
