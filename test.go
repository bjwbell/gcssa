package main

func test0() int {
	return -1
}

func test1() int {
	var vk int
	vk = 11
	return vk
}

func test() int {
	var vk int
	x := vk == 0
	if x {
		return vk
	} else {
		return -1
	}
	/*if vk == 0 {
		return vk
	} else {
		return -1
	}*/
}
