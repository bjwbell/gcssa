package gcssa

func countfield(t *Type) int {
	n := 0
	for t1 := t.Type; t1 != nil; t1 = t1.Down {
		n++
	}
	return n
}
