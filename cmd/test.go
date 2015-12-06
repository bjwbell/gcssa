package main

func test0() {
}

func test1() {
	return
}

func test2() int {
	return 1
}

func test3() int {
	return -1
}

func test4(x int) int {
	return x
}

func test5() int {
	var vk int
	vk = 11
	return vk
}

func test6() int {
	var vk int
	x := vk == 0
	if x {
		return vk
	} else {
		return -1
	}
}

func test7() int {
	var vk int
	if vk == 0 {
		return vk
	} else {
		return -1
	}
}
