package gcssa

import (
	"fmt"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"

	"github.com/bjwbell/ssa"
)

type phi struct {
	node ast.Node
}

type ssaVar struct {
	name string
	node ast.Node
}

type fnSSA struct {
	removedPhi []phi
	vars       []ssaVar
	fnDecl     *ast.FuncDecl
}

func (fn *fnSSA) removePhi() bool {
	return true
}

func (fn *fnSSA) rewriteAssign() bool {
	return true
}

func (fn *fnSSA) restorePhi() bool {
	return true
}

// ParseSSA parses the function, fn, which must be in ssa form and returns
// the corresponding ssa.Func
func ParseSSA(file, pkgName, fn string) (ssafn *ssa.Func, usessa bool) {
	var conf types.Config
	conf.Importer = importer.Default()
	conf.Error = func(err error) {
		fmt.Println("terror:", err)
	}
	fset := token.NewFileSet()
	fileAst, err := parser.ParseFile(fset, file, nil, parser.AllErrors)
	fileTok := fset.File(fileAst.Pos())
	var terrors string
	if err != nil {
		fmt.Printf("Error parsing %v, error message: %v\n", file, err)
		terrors += fmt.Sprintf("err: %v\n", err)
		return
	}

	ast.FilterFile(fileAst, func(declName string) bool {
		return declName == fn
	})

	var fnDcl *ast.FuncDecl
	for _, decl := range fileAst.Decls {
		if fdecl, ok := decl.(*ast.FuncDecl); ok {
			fnDcl = fdecl
		}
	}

	if fnDcl == nil {
		fmt.Printf("Error \"%v\" not found", fn)
		return
	}

	fnSSA := fnSSA{fnDecl: fnDcl, removedPhi: []phi{}, vars: []ssaVar{}}

	if !fnSSA.removePhi() {
		fmt.Printf("Error rewriting phi vars")
		return
	}
	if !fnSSA.rewriteAssign() {
		fmt.Printf("Error rewriting assignments")
		return
	}
	if !fnSSA.restorePhi() {
		fmt.Printf("Error rewriting phi vars")
		return
	}

	files := []*ast.File{fileAst}
	info := types.Info{
		Types: make(map[ast.Expr]types.TypeAndValue),
		Defs:  make(map[*ast.Ident]types.Object),
		Uses:  make(map[*ast.Ident]types.Object),
	}
	pkg, err := conf.Check(pkgName, fset, files, &info)
	if err != nil {
		if terrors != fmt.Sprintf("err: %v\n", err) {
			fmt.Printf("Type error (%v) message: %v\n", file, err)
			return
		}
	}

	fmt.Println("pkg: ", pkg)
	fmt.Println("pkg.Complete:", pkg.Complete())
	scope := pkg.Scope()
	obj := scope.Lookup(fn)
	if obj == nil {
		fmt.Println("Couldnt lookup function: ", fn)
		return
	}
	function, ok := obj.(*types.Func)
	if !ok {
		fmt.Printf("%v is a %v, not a function\n", fn, obj.Type().String())
	}
	var fnDecl *ast.FuncDecl
	for _, decl := range fileAst.Decls {
		if fdecl, ok := decl.(*ast.FuncDecl); ok {
			if fdecl.Name.Name == fn {
				fnDecl = fdecl
				break
			}
		}
	}
	if fnDecl == nil {
		fmt.Println("couldn't find function: ", fn)
		return
	}
	ssafn, ok = parseSSA(fileTok, fileAst, fnDecl, function, &info)
	if ssafn == nil || !ok {
		fmt.Println("Error building SSA form")
	} else {
		fmt.Println("ssa:\n", ssafn)
	}
	if ssafn != nil && ok {
		fmt.Println("ssafn:", ssafn)
	}
	return ssafn, ok
}

func parseSSA(ftok *token.File, f *ast.File, fn *ast.FuncDecl, fnType *types.Func, fnInfo *types.Info) (ssafn *ssa.Func, ok bool) {
	return nil, false
}
