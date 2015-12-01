// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"bytes"
	"fmt"
	"math/big"
)

// avoid <ctype.h>

// The parser's maximum stack size.
// We have to use a #define macro here since yacc
// or bison will check for its definition and use
// a potentially smaller value if it is undefined.
const (
	NHUNK           = 50000
	BUFSIZ          = 8192
	NSYMB           = 500
	NHASH           = 1024
	MAXALIGN        = 7
	UINF            = 100
	PRIME1          = 3
	BADWIDTH        = -1000000000
	MaxStackVarSize = 10 * 1024 * 1024
)

const (
	// These values are known by runtime.
	// The MEMx and NOEQx values must run in parallel.  See algtype.
	AMEM = iota
	AMEM0
	AMEM8
	AMEM16
	AMEM32
	AMEM64
	AMEM128
	ANOEQ
	ANOEQ0
	ANOEQ8
	ANOEQ16
	ANOEQ32
	ANOEQ64
	ANOEQ128
	ASTRING
	AINTER
	ANILINTER
	ASLICE
	AFLOAT32
	AFLOAT64
	ACPLX64
	ACPLX128
	AUNK = 100
)

const (
	// Maximum size in bits for Mpints before signalling
	// overflow and also mantissa precision for Mpflts.
	Mpprec = 512
	// Turn on for constant arithmetic debugging output.
	Mpdebug = false
)

// Mpint represents an integer constant.
type Mpint struct {
	Val  big.Int
	Ovf  bool // set if Val overflowed compiler limit (sticky)
	Rune bool // set if syntax indicates default type rune
}

// Mpflt represents a floating-point constant.
type Mpflt struct {
	Val big.Float
}

// Mpcplx represents a complex constant.
type Mpcplx struct {
	Real Mpflt
	Imag Mpflt
}

type Val struct {
	// U contains one of:
	// bool     bool when n.ValCtype() == CTBOOL
	// *Mpint   int when n.ValCtype() == CTINT, rune when n.ValCtype() == CTRUNE
	// *Mpflt   float when n.ValCtype() == CTFLT
	// *Mpcplx  pair of floats when n.ValCtype() == CTCPLX
	// string   string when n.ValCtype() == CTSTR
	// *Nilval  when n.ValCtype() == CTNIL
	U interface{}
}

type NilVal struct{}

func (v Val) Ctype() int {
	switch x := v.U.(type) {
	default:
		panic(fmt.Sprintf("unexpected Ctype for %T", v.U))
	case nil:
		return 0
	case *NilVal:
		return CTNIL
	case bool:
		return CTBOOL
	case *Mpint:
		if x.Rune {
			return CTRUNE
		}
		return CTINT
	case *Mpflt:
		return CTFLT
	case *Mpcplx:
		return CTCPLX
	case string:
		return CTSTR
	}
}

type Sym struct {
	Lexical  uint16
	Flags    uint8
	Link     *Sym
	Uniqgen  uint32
	Linkname string // link name

	// saved and restored by dcopy
	Name string // variable name
	Def  *Node  // definition: ONAME OTYPE OPACK or OLITERAL
	//Label      *Label // corresponding label (ephemeral)
	Block      int32 // blocknumber to catch redeclaration
	Lastlineno int32 // last declaration for diagnostic
	//Lsym       *obj.LSym
	Fsym *Sym // funcsym
}

func (s Sym) String() string {
	return "<Sym>"
}

/*type Label struct {
	Sym  *Sym
	Def  *Node
	Use  []*Node
	Link *Label

	// for use during gen
	Gotopc   *obj.Prog // pointer to unresolved gotos
	Labelpc  *obj.Prog // pointer to code
	Breakpc  *obj.Prog // pointer to code
	Continpc *obj.Prog // pointer to code

	Used bool
}*/

type InitEntry struct {
	Xoffset int64 // struct, array only
	Expr    *Node // bytes of run-time computed expressions
}

type InitPlan struct {
	Lit  int64
	Zero int64
	Expr int64
	E    []InitEntry
}

const (
	SymExport   = 1 << 0 // to be exported
	SymPackage  = 1 << 1
	SymExported = 1 << 2 // already written out by export
	SymUniq     = 1 << 3
	SymSiggen   = 1 << 4
	SymAsm      = 1 << 5
	SymAlgGen   = 1 << 6
)

var dclstack *Sym

const (
	Txxx = iota

	TINT8
	TUINT8
	TINT16
	TUINT16
	TINT32
	TUINT32
	TINT64
	TUINT64
	TINT
	TUINT
	TUINTPTR

	TCOMPLEX64
	TCOMPLEX128

	TFLOAT32
	TFLOAT64

	TBOOL

	TPTR32
	TPTR64

	TFUNC
	TARRAY
	T_old_DARRAY
	TSTRUCT
	TCHAN
	TMAP
	TINTER
	TFIELD
	TANY
	TSTRING
	TUNSAFEPTR

	// pseudo-types for literals
	TIDEAL
	TNIL
	TBLANK

	// pseudo-type for frame layout
	TFUNCARGS
	TCHANARGS
	TINTERMETH

	NTYPE
)

const (
	CTxxx = iota

	CTINT
	CTRUNE
	CTFLT
	CTCPLX
	CTSTR
	CTBOOL
	CTNIL
)

const (
	/* types of channel */
	/* must match ../../pkg/nreflect/type.go:/Chandir */
	Cxxx  = 0
	Crecv = 1 << 0
	Csend = 1 << 1
	Cboth = Crecv | Csend
)

// declaration context
const (
	Pxxx      = uint8(iota)
	PEXTERN   // global variable
	PAUTO     // local variables
	PPARAM    // input arguments
	PPARAMOUT // output results
	PPARAMREF // closure variable reference
	PFUNC     // global function

	PDISCARD // discard during parse of duplicate import

	PHEAP = uint8(1 << 7) // an extra bit to identify an escaped variable
)

const (
	Etop      = 1 << 1 // evaluated at statement level
	Erv       = 1 << 2 // evaluated in value context
	Etype     = 1 << 3
	Ecall     = 1 << 4  // call-only expressions are ok
	Efnstruct = 1 << 5  // multivalue function returns are ok
	Eiota     = 1 << 6  // iota is ok
	Easgn     = 1 << 7  // assigning to expression
	Eindir    = 1 << 8  // indirecting through expression
	Eaddr     = 1 << 9  // taking address of expression
	Eproc     = 1 << 10 // inside a go statement
	Ecomplit  = 1 << 11 // type in composite literal
)

type Typedef struct {
	Name   string
	Etype  int
	Sameas int
}

/*
 * argument passing to/from
 * smagic and umagic
 */
type Magic struct {
	W   int // input for both - width
	S   int // output for both - shift
	Bad int // output for both - unexpected failure

	// magic multiplier for signed literal divisors
	Sd int64 // input - literal divisor
	Sm int64 // output - multiplier

	// magic multiplier for unsigned literal divisors
	Ud uint64 // input - literal divisor
	Um uint64 // output - multiplier
	Ua int    // output - adder
}

/*
 * note this is the runtime representation
 * of the compilers arrays.
 *
 * typedef	struct
 * {				// must not move anything
 *	uchar	array[8];	// pointer to data
 *	uchar	nel[4];		// number of elements
 *	uchar	cap[4];		// allocated number of elements
 * } Array;
 */
var Array_array int // runtime offsetof(Array,array) - same for String

var Array_nel int // runtime offsetof(Array,nel) - same for String

var Array_cap int // runtime offsetof(Array,cap)

var sizeof_Array int // runtime sizeof(Array)

/*
 * note this is the runtime representation
 * of the compilers strings.
 *
 * typedef	struct
 * {				// must not move anything
 *	uchar	array[8];	// pointer to data
 *	uchar	nel[4];		// number of elements
 * } String;
 */
var sizeof_String int // runtime sizeof(String)

var lexlineno int32

var lineno int32

var prevlineno int32

var pragcgobuf string

var infile string

var outfile string

var nerrors int

var nsavederrors int

var nsyntaxerrors int

var decldepth int32

var safemode int

var nolocalimports int

var lexbuf bytes.Buffer
var strbuf bytes.Buffer

var litbuf string

var Debug [256]int

var debugstr string

var Debug_checknil int
var Debug_typeassert int

var importmyname *Sym // my name for package

var Tptr int // either TPTR32 or TPTR64

var myimportpath string

var localimport string

var asmhdr string

var Types [NTYPE]*Type

var idealstring *Type

var idealbool *Type

var bytetype *Type

var runetype *Type

var errortype *Type

var Simtype [NTYPE]uint8

var (
	Isptr     [NTYPE]bool
	Isint     [NTYPE]bool
	Isfloat   [NTYPE]bool
	Iscomplex [NTYPE]bool
	Issigned  [NTYPE]bool
	issimple  [NTYPE]bool
)

var (
	okforeq    [NTYPE]bool
	okforadd   [NTYPE]bool
	okforand   [NTYPE]bool
	okfornone  [NTYPE]bool
	okforcmp   [NTYPE]bool
	okforbool  [NTYPE]bool
	okforcap   [NTYPE]bool
	okforlen   [NTYPE]bool
	okforarith [NTYPE]bool
	okforconst [NTYPE]bool
)

var (
	okfor [OEND][]bool
	iscmp [OEND]bool
)

var Minintval [NTYPE]*Mpint

var Maxintval [NTYPE]*Mpint

var minfltval [NTYPE]*Mpflt

var maxfltval [NTYPE]*Mpflt

var xtop *NodeList

var externdcl []*Node

var exportlist []*Node

var importlist []*Node // imported functions and methods with inlinable bodies

var funcsyms []*Node

var dclcontext uint8 // PEXTERN/PAUTO

var incannedimport int

var statuniqgen int // name generator for static temps

var loophack bool

var iota_ int32

var lastconst *NodeList

var lasttype *Node

var Maxarg int64

var Stksize int64 // stack size for current frame

var stkptrsize int64 // prefix of stack containing pointers

var blockgen int32 // max block number

var block int32 // current block number

var hasdefer bool // flag that curfn has defer statement

var Curfn *Node

var Widthptr int = 8

var Widthint int = 8

var Widthreg int

var typesw *Node

var nblank *Node

var Funcdepth int32

var typecheckok bool

var compiling_runtime int

var compiling_wrappers int

var use_writebarrier int

var pure_go int

var flag_installsuffix string

var flag_race int

var flag_largemodel int

// Pending annotations for next func declaration.
var (
	noescape       bool
	nosplit        bool
	nowritebarrier bool
	systemstack    bool
	norace         bool
)

var debuglive int

//var Ctxt *obj.Link

var nointerface bool

var writearchive int

//var bstdout obj.Biobuf

var Nacl bool

//var continpc *obj.Prog

//var breakpc *obj.Prog

var Pc *Prog

var nodfp *Node

var Disable_checknil int

type Flow struct {
	//Prog   *obj.Prog // actual instruction
	P1     *Flow // predecessors of this instruction: p1,
	P2     *Flow // and then p2 linked though p2link.
	P2link *Flow
	S1     *Flow // successors of this instruction (at most two: s1 and s2).
	S2     *Flow
	Link   *Flow // next instruction in function code

	Active int32 // usable by client

	Id     int32  // sequence number in flow graph
	Rpo    int32  // reverse post ordering
	Loop   uint16 // x5 for every loop
	Refset bool   // diagnostic generated

	Data interface{} // for use by client
}

type Graph struct {
	Start *Flow
	Num   int

	// After calling flowrpo, rpo lists the flow nodes in reverse postorder,
	// and each non-dead Flow node f has g->rpo[f->rpo] == f.
	Rpo []*Flow
}

/*
 *	interface to back end
 */

const (
	// Pseudo-op, like TEXT, GLOBL, TYPE, PCDATA, FUNCDATA.
	Pseudo = 1 << 1

	// There's nothing to say about the instruction,
	// but it's still okay to see.
	OK = 1 << 2

	// Size of right-side write, or right-side read if no write.
	SizeB = 1 << 3
	SizeW = 1 << 4
	SizeL = 1 << 5
	SizeQ = 1 << 6
	SizeF = 1 << 7
	SizeD = 1 << 8

	// Left side (Prog.from): address taken, read, write.
	LeftAddr  = 1 << 9
	LeftRead  = 1 << 10
	LeftWrite = 1 << 11

	// Register in middle (Prog.reg); only ever read. (arm, ppc64)
	RegRead    = 1 << 12
	CanRegRead = 1 << 13

	// Right side (Prog.to): address taken, read, write.
	RightAddr  = 1 << 14
	RightRead  = 1 << 15
	RightWrite = 1 << 16

	// Instruction kinds
	Move  = 1 << 17 // straight move
	Conv  = 1 << 18 // size conversion
	Cjmp  = 1 << 19 // conditional jump
	Break = 1 << 20 // breaks control flow (no fallthrough)
	Call  = 1 << 21 // function call
	Jump  = 1 << 22 // jump
	Skip  = 1 << 23 // data instruction

	// Set, use, or kill of carry bit.
	// Kill means we never look at the carry bit after this kind of instruction.
	SetCarry  = 1 << 24
	UseCarry  = 1 << 25
	KillCarry = 1 << 26

	// Special cases for register use. (amd64, 386)
	ShiftCX  = 1 << 27 // possible shift by CX
	ImulAXDX = 1 << 28 // possible multiply into DX:AX

	// Instruction updates whichever of from/to is type D_OREG. (ppc64)
	PostInc = 1 << 29
)

type Arch struct {
	Thechar   int
	Thestring string
	//Thelinkarch  *obj.LinkArch
	Typedefs     []Typedef
	REGSP        int
	REGCTXT      int
	REGCALLX     int // BX
	REGCALLX2    int // AX
	REGRETURN    int // AX
	REGMIN       int
	REGMAX       int
	REGZERO      int // architectural zero register, if available
	FREGMIN      int
	FREGMAX      int
	MAXWIDTH     int64
	ReservedRegs []int

	AddIndex   func(*Node, int64, *Node) bool // optional
	Betypeinit func()
	//Bgen_float   func(*Node, bool, int, *obj.Prog) // optional
	Cgen64 func(*Node, *Node) // only on 32-bit systems
	//Cgenindex    func(*Node, *Node, bool) *obj.Prog
	Cgen_bmul  func(int, *Node, *Node, *Node) bool
	Cgen_float func(*Node, *Node) // optional
	Cgen_hmul  func(*Node, *Node, *Node)
	Cgen_shift func(int, bool, *Node, *Node, *Node)
	Clearfat   func(*Node)
	//Cmp64      func(*Node, *Node, int, int, *obj.Prog) // only on 32-bit systems
	//Defframe     func(*obj.Prog)
	Dodiv  func(int, *Node, *Node, *Node)
	Excise func(*Flow)
	//Expandchecks func(*obj.Prog)
	Getg func(*Node)
	//Gins         func(int, *Node, *Node) *obj.Prog

	// Ginscmp generates code comparing n1 to n2 and jumping away if op is satisfied.
	// The returned prog should be Patch'ed with the jump target.
	// If op is not satisfied, code falls through to the next emitted instruction.
	// Likely is the branch prediction hint: +1 for likely, -1 for unlikely, 0 for no opinion.
	//
	// Ginscmp must be able to handle all kinds of arguments for n1 and n2,
	// not just simple registers, although it can assume that there are no
	// function calls needed during the evaluation, and on 32-bit systems
	// the values are guaranteed not to be 64-bit values, so no in-memory
	// temporaries are necessary.
	//Ginscmp func(op int, t *Type, n1, n2 *Node, likely int) *obj.Prog

	// Ginsboolval inserts instructions to convert the result
	// of a just-completed comparison to a boolean value.
	// The first argument is the conditional jump instruction
	// corresponding to the desired value.
	// The second argument is the destination.
	// If not present, Ginsboolval will be emulated with jumps.
	Ginsboolval func(int, *Node)

	Ginscon func(int, int64, *Node)
	Ginsnop func()
	Gmove   func(*Node, *Node)
	//Igenindex    func(*Node, *Node, bool) *obj.Prog
	Linkarchinit func()
	/*Peep         func(*obj.Prog)
	Proginfo     func(*obj.Prog) // fills in Prog.Info
	Regtyp       func(*obj.Addr) bool
	Sameaddr     func(*obj.Addr, *obj.Addr) bool
	Smallindir   func(*obj.Addr, *obj.Addr) bool
	Stackaddr    func(*obj.Addr) bool
	Blockcopy    func(*Node, *Node, int64, int64, int64)
	Sudoaddable  func(int, *Node, *obj.Addr) bool*/
	Sudoclean    func()
	Excludedregs func() uint64
	RtoB         func(int) uint64
	FtoB         func(int) uint64
	BtoR         func(uint64) int
	BtoF         func(uint64) int
	Doregbits    func(int) uint64
	Regnames     func(*int) []string
	Use387       bool // should 8g use 387 FP instructions instead of sse2.
}

var pcloc int32

var Thearch Arch

var Newproc *Node

var Deferproc *Node

var Deferreturn *Node

var Panicindex *Node

var panicslice *Node

var panicdivide *Node

var throwreturn *Node

var growslice *Node

var typedmemmove_nostore *Node

var panicdottype *Node
