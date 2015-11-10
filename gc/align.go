package gc

func Rnd(o int64, r int64) int64 {
	if r < 1 || r > 8 || r&(r-1) != 0 {
		panic("rnd d")
	}
	return (o + r - 1) &^ (r - 1)
}
