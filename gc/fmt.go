package gc

// Fmt "%O":  Node opcodes
func Oconv(o int, flag int) string {
	return "<Oconv>"
	/*if (flag&obj.FmtSharp != 0) || fmtmode != FDbg {
		if o >= 0 && o < len(goopnames) && goopnames[o] != "" {
			return goopnames[o]
		}
	}

	if o >= 0 && o < len(opnames) && opnames[o] != "" {
		return opnames[o]
	}

	return fmt.Sprintf("O-%d", o)*/
}

// Fmt "%E": etype
func Econv(et int, flag int) string {
	return "<Econv>"
	/*if et >= 0 && et < len(etnames) && etnames[et] != "" {
		return etnames[et]
	}
	return fmt.Sprintf("E-%d", et)*/
}
