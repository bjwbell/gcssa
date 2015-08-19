// run

// Copyright 2015 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Tests arithmetic expressions

package main

// test64BitConstMult tests that rewrite rules don't fold 64 bit constants
// into multiply instructions.
func test64BitConstMult() {
	want := int64(103079215109)
	if got := test64BitConstMult_ssa(1, 2); want != got {
		println("test64BitConstMult failed, wanted", want, "got", got)
		failed = true
	}
}
func test64BitConstMult_ssa(a, b int64) int64 {
	switch { // prevent inlining
	}
	return 34359738369*a + b*34359738370
}

// test64BitConstAdd tests that rewrite rules don't fold 64 bit constants
// into add instructions.
func test64BitConstAdd() {
	want := int64(3567671782835376650)
	if got := test64BitConstAdd_ssa(1, 2); want != got {
		println("test64BitConstAdd failed, wanted", want, "got", got)
		failed = true
	}
}
func test64BitConstAdd_ssa(a, b int64) int64 {
	switch { // prevent inlining
	}
	return a + 575815584948629622 + b + 2991856197886747025
}

// testRegallocCVSpill tests that regalloc spills a value whose last use is the
// current value.
func testRegallocCVSpill() {
	want := int8(-9)
	if got := testRegallocCVSpill_ssa(1, 2, 3, 4); want != got {
		println("testRegallocCVSpill failed, wanted", want, "got", got)
		failed = true
	}
}
func testRegallocCVSpill_ssa(a, b, c, d int8) int8 {
	switch { // prevent inlining
	}
	return a + -32 + b + 63*c*-87*d
}

func testBitwiseLogic() {
	a, b := uint32(57623283), uint32(1314713839)
	if want, got := uint32(38551779), testBitwiseAnd_ssa(a, b); want != got {
		println("testBitwiseAnd failed, wanted", want, "got", got)
		failed = true
	}
	if want, got := uint32(1333785343), testBitwiseOr_ssa(a, b); want != got {
		println("testBitwiseOr failed, wanted", want, "got", got)
		failed = true
	}
	if want, got := uint32(1295233564), testBitwiseXor_ssa(a, b); want != got {
		println("testBitwiseXor failed, wanted", want, "got", got)
		failed = true
	}
	if want, got := int32(832), testBitwiseLsh_ssa(13, 4, 2); want != got {
		println("testBitwiseLsh failed, wanted", want, "got", got)
		failed = true
	}
	if want, got := int32(0), testBitwiseLsh_ssa(13, 25, 15); want != got {
		println("testBitwiseLsh failed, wanted", want, "got", got)
		failed = true
	}
	if want, got := int32(0), testBitwiseLsh_ssa(-13, 25, 15); want != got {
		println("testBitwiseLsh failed, wanted", want, "got", got)
		failed = true
	}
	if want, got := int32(-13), testBitwiseRsh_ssa(-832, 4, 2); want != got {
		println("testBitwiseRsh failed, wanted", want, "got", got)
		failed = true
	}
	if want, got := int32(0), testBitwiseRsh_ssa(13, 25, 15); want != got {
		println("testBitwiseRsh failed, wanted", want, "got", got)
		failed = true
	}
	if want, got := int32(-1), testBitwiseRsh_ssa(-13, 25, 15); want != got {
		println("testBitwiseRsh failed, wanted", want, "got", got)
		failed = true
	}
	if want, got := uint32(0x3ffffff), testBitwiseRshU_ssa(0xffffffff, 4, 2); want != got {
		println("testBitwiseRshU failed, wanted", want, "got", got)
		failed = true
	}
	if want, got := uint32(0), testBitwiseRshU_ssa(13, 25, 15); want != got {
		println("testBitwiseRshU failed, wanted", want, "got", got)
		failed = true
	}
	if want, got := uint32(0), testBitwiseRshU_ssa(0x8aaaaaaa, 25, 15); want != got {
		println("testBitwiseRshU failed, wanted", want, "got", got)
		failed = true
	}
}

func testBitwiseAnd_ssa(a, b uint32) uint32 {
	switch { // prevent inlining
	}
	return a & b
}

func testBitwiseOr_ssa(a, b uint32) uint32 {
	switch { // prevent inlining
	}
	return a | b
}

func testBitwiseXor_ssa(a, b uint32) uint32 {
	switch { // prevent inlining
	}
	return a ^ b
}

func testBitwiseLsh_ssa(a int32, b, c uint32) int32 {
	switch { // prevent inlining
	}
	return a << b << c
}

func testBitwiseRsh_ssa(a int32, b, c uint32) int32 {
	switch { // prevent inlining
	}
	return a >> b >> c
}

func testBitwiseRshU_ssa(a uint32, b, c uint32) uint32 {
	switch { // prevent inlining
	}
	return a >> b >> c
}

func testShiftCX_ssa() int {
	switch {
	} // prevent inlining
	v1 := uint8(3)
	v4 := (v1 * v1) ^ v1 | v1 - v1 - v1&v1 ^ uint8(3+2) + v1*1>>0 - v1 | 1 | v1<<(2*3|0-0*0^1)
	v5 := v4>>(3-0-uint(3)) | v1 | v1 + v1 ^ v4<<(0+1|3&1)<<(uint64(1)<<0*2*0<<0) ^ v1
	v6 := v5 ^ (v1+v1)*v1 | v1 | v1*v1>>(v1&v1)>>(uint(1)<<0*uint(3)>>1)*v1<<2*v1<<v1 - v1>>2 | (v4 - v1) ^ v1 + v1 ^ v1>>1 | v1 + v1 - v1 ^ v1
	v7 := v6 & v5 << 0
	v1++
	v11 := 2&1 ^ 0 + 3 | int(0^0)<<1>>(1*0*3) ^ 0*0 ^ 3&0*3&3 ^ 3*3 ^ 1 ^ int(2)<<(2*3) + 2 | 2 | 2 ^ 2 + 1 | 3 | 0 ^ int(1)>>1 ^ 2 // int
	v7--
	return int(uint64(2*1)<<(3-2)<<uint(3>>v7)-2)&v11 | v11 - int(2)<<0>>(2-1)*(v11*0&v11<<1<<(uint8(2)+v4))
}

func testShiftCX() {
	want := 141
	if got := testShiftCX_ssa(); want != got {
		println("testShiftCX failed, wanted", want, "got", got)
		failed = true
	}
}

// testSubqToNegq ensures that the SUBQ -> NEGQ translation works correctly.
func testSubqToNegq() {
	want := int64(-318294940372190156)
	if got := testSubqToNegq_ssa(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2); want != got {
		println("testSubqToNegq failed, wanted", want, "got", got)
		failed = true
	}
}

func testSubqToNegq_ssa(a, b, c, d, e, f, g, h, i, j, k int64) int64 {
	switch { // prevent inlining
	}
	return a + 8207351403619448057 - b - 1779494519303207690 + c*8810076340510052032*d - 4465874067674546219 - e*4361839741470334295 - f + 8688847565426072650*g*8065564729145417479
}

func testOcom() {
	want1, want2 := int32(0x55555555), int32(-0x55555556)
	if got1, got2 := testOcom_ssa(0x55555555, 0x55555555); want1 != got1 || want2 != got2 {
		println("testSubqToNegq failed, wanted", want1, "and", want2,
			"got", got1, "and", got2)
		failed = true
	}
}

func testOcom_ssa(a, b int32) (int32, int32) {
	switch { // prevent inlining
	}
	return ^^^^a, ^^^^^b
}

func lrot1_ssa(w uint8, x uint16, y uint32, z uint64) (a uint8, b uint16, c uint32, d uint64) {
	a = (w << 5) | (w >> 3)
	b = (x << 13) | (x >> 3)
	c = (y << 29) | (y >> 3)
	d = (z << 61) | (z >> 3)
	return
}

func lrot2_ssa(w, n uint32) uint32 {
	// Want to be sure that a "rotate by 32" which
	// is really 0 | (w >> 0) == w
	// is correctly compiled.
	switch { // prevents inlining
	}
	return (w << n) | (w >> (32 - n))
}

func lrot3_ssa(w uint32) uint32 {
	// Want to be sure that a "rotate by 32" which
	// is really 0 | (w >> 0) == w
	// is correctly compiled.
	switch { // prevents inlining
	}
	return (w << 32) | (w >> (32 - 32))
}

func testLrot() {
	wantA, wantB, wantC, wantD := uint8(0xe1), uint16(0xe001),
		uint32(0xe0000001), uint64(0xe000000000000001)
	a, b, c, d := lrot1_ssa(0xf, 0xf, 0xf, 0xf)
	if a != wantA || b != wantB || c != wantC || d != wantD {
		println("lrot1_ssa(0xf, 0xf, 0xf, 0xf)=",
			wantA, wantB, wantC, wantD, ", got", a, b, c, d)
		failed = true
	}
	x := lrot2_ssa(0xb0000001, 32)
	wantX := uint32(0xb0000001)
	if x != wantX {
		println("lrot2_ssa(0xb0000001, 32)=",
			wantX, ", got", x)
		failed = true
	}
	x = lrot3_ssa(0xb0000001)
	if x != wantX {
		println("lrot3_ssa(0xb0000001)=",
			wantX, ", got", x)
		failed = true
	}

}

func sub1_ssa() uint64 {
	switch {
	} // prevent inlining
	v1 := uint64(3) // uint64
	return v1*v1 - (v1&v1)&v1
}
func sub2_ssa() uint8 {
	switch {
	}
	v1 := uint8(0)
	v3 := v1 + v1 + v1 ^ v1 | 3 + v1 ^ v1 | v1 ^ v1
	v1-- // dev.ssa doesn't see this one
	return v1 ^ v1*v1 - v3
}

func testSubConst() {
	x1 := sub1_ssa()
	want1 := uint64(6)
	if x1 != want1 {
		println("sub1_ssa()=", want1, ", got", x1)
		failed = true
	}
	x2 := sub2_ssa()
	want2 := uint8(251)
	if x2 != want2 {
		println("sub2_ssa()=", want2, ", got", x2)
		failed = true
	}
}

var failed = false

func main() {

	test64BitConstMult()
	test64BitConstAdd()
	testRegallocCVSpill()
	testSubqToNegq()
	testBitwiseLogic()
	testOcom()
	testLrot()
	testShiftCX()
	testSubConst()

	if failed {
		panic("failed")
	}
}
