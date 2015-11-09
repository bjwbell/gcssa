package gcssa

import "math"

func mpgetflt(a *Mpflt) float64 {
	x, _ := a.Val.Float64()

	// check for overflow
	if math.IsInf(x, 0) {
		panic("mpgetflt ovf")
	}

	return x
}

func mpgetflt32(a *Mpflt) float64 {
	x32, _ := a.Val.Float32()
	x := float64(x32)

	// check for overflow
	if math.IsInf(x, 0) {
		panic("mpgetflt32 ovf")
	}

	return x
}
