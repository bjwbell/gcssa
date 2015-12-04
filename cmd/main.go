package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"log"
	"os"
	"strings"

	"github.com/bjwbell/gcssa"
)

func filePath(pathName string) string {
	split := strings.Split(pathName, "/")
	dir := ""
	if len(split) == 1 {
		dir = "."
	} else if len(split) == 2 {
		dir = split[0] + "/"
	} else {
		dir = strings.Join(split[0:len(split)-2], "/")
	}
	return dir
}

func main() {
	var pkgName = flag.String("pkg", "", "input file package name")
	var f = flag.String("f", "", "input file with function definitions")
	var fn = flag.String("fn", "", "function name")
	flag.Parse()

	file := os.ExpandEnv("$GOFILE")
	log.SetFlags(log.Lshortfile)
	if *f != "" {
		file = *f
	}
	if *fn == "" {
		log.Fatalf("Error no function name(s) provided")
	}
	if *pkgName == "" {
		*pkgName = filePath(file)
	}

	var conf types.Config
	conf.Importer = importer.Default()

	fset := token.NewFileSet()
	fileAst, err := parser.ParseFile(fset, file, nil, parser.AllErrors)
	fileTok := fset.File(fileAst.Pos())
	if err != nil {
		fmt.Printf("Error parsing %v, error message: %v\n", file, err)
		return
	}
	files := []*ast.File{fileAst}
	info := types.Info{
		Types: make(map[ast.Expr]types.TypeAndValue),
		Defs:  make(map[*ast.Ident]types.Object),
		Uses:  make(map[*ast.Ident]types.Object),
	}
	pkg, err := conf.Check(*pkgName, fset, files, &info)
	if err != nil {
		fmt.Printf("Error type checking %v, error message: %v\n", file, err)
		return
	}

	fmt.Println("pkg: ", pkg)
	fmt.Println("pkg.Complete:", pkg.Complete())
	scope := pkg.Scope()
	obj := scope.Lookup(*fn)
	if obj == nil {
		fmt.Println("Couldnt lookup function: ", *fn)
		return
	}
	function, ok := obj.(*types.Func)
	if !ok {
		fmt.Printf("%v is a %v, not a function\n", *fn, obj.Type().String())
	}
	var fnDecl *ast.FuncDecl
	for _, decl := range fileAst.Decls {
		if fdecl, ok := decl.(*ast.FuncDecl); ok {
			if fdecl.Name.Name == *fn {
				fnDecl = fdecl
				break
			}
		}
	}
	if fnDecl == nil {
		fmt.Println("couldn't find function: ", *fn)
		return
	}
	ssafn, ok := gcssa.BuildSSA(fileTok, fileAst, fnDecl, function, &info)
	if ssafn == nil || !ok {
		fmt.Println("Error building SSA form")
	} else {
		fmt.Println("ssa:\n", ssafn)
	}

	if ssafn != nil && ok {
		fnProgs, ok := gcssa.GenSSA(ssafn)
		if !ok {
			fmt.Println("Error creating assembly for SSA")
		} else {
			fmt.Printf("ssa assembly:\n%s", gcssa.Assemble(fnProgs))
		}
	}
}
