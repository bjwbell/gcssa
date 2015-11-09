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

	"github.com/bjwbell/gcssa/gc"
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

	var conf types.Config
	conf.Importer = importer.Default()
	fset := token.NewFileSet()
	fileAst, err := parser.ParseFile(fset, file, nil, parser.AllErrors)
	if err != nil {
		fmt.Printf("Error parsing %v, error message: %v\n", file, err)
		return
	}
	files := []*ast.File{fileAst}
	pkg, err := conf.Check(filePath(file), fset, files, nil)
	if err != nil {
		fmt.Printf("Error type checking %v, error message: %v\n", file, err)
		return
	}

	fmt.Println("pkg: ", pkg)
	fmt.Println("pkg.Name: ", pkg.Name())
	fmt.Println("pkg.Path: ", pkg.Path())
	fmt.Println("pkg.Complete:", pkg.Complete())
	scope := pkg.Scope()
	obj := scope.Lookup(*fn)
	if obj == nil {
		fmt.Println("Couldnt lookup function: ", *fn)
		return
	}
	fmt.Println("obj: ", obj)
	fmt.Println("obj.Id: ", obj.Id())
	fmt.Println("obj.Type: ", obj.Type())
	typ := obj.Type()
	function, ok := obj.(*types.Func)
	if !ok {
		fmt.Printf("Function %v is of type %v\n", *fn, typ.String())
	}
	fmt.Println("function: ", function)
	node := ConvertFnToNode(function)
	fmt.Println("node: ", node)
}

func ConvertFnToNode(fn *types.Func) *gcssa.Node {
	return nil
}
