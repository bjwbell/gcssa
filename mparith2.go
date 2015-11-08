package gcssa

func Mpgetfix(a *Mpint) int64 {
	if a.Ovf {
		panic("constant overflow")
	}
	return a.Val.Int64()
}
