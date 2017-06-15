/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Ugorji Nwoke
 * *reset*
 */

package main

import (
	"fmt"
	"os"
	"strconv"
)

const minDepth = 4

type treeNode struct {
	left, right *treeNode
}

func (n *treeNode) itemCheck() int {
	if n.left == nil {
		return 1
	}
	return 1 + n.left.itemCheck() + n.right.itemCheck()
}

func bottomUp(depth int) *treeNode {
	if depth > 0 {
		return &treeNode{
			bottomUp(depth - 1),
			bottomUp(depth - 1),
		}
	}
	return &treeNode{nil, nil}
}

func main() {
	n := 0
	if len(os.Args) > 1 {
		if n2, err2 := strconv.ParseInt(os.Args[1], 10, 0); err2 == nil {
			n = int(n2)
		}
	}
	maxDepth := n
	if minDepth+2 > n {
		maxDepth = minDepth + 2
	}
	stretchDepth := maxDepth + 1
	check := bottomUp(stretchDepth).itemCheck()
	fmt.Printf("stretch tree of depth %v\t check: %v\n", stretchDepth, check)
	longLivedTree := bottomUp(maxDepth)
	for depth := minDepth; depth <= maxDepth; depth += 2 {
		interactions := 1 << uint(maxDepth-depth+minDepth)
		check = 0
		for i := 1; i <= interactions; i++ {
			check += bottomUp(depth).itemCheck()
		}
		fmt.Printf("%v\t trees of depth %v\t check: %v\n", interactions, depth, check)
	}
	fmt.Printf("long lived tree of depth %v\t check: %v\n", maxDepth, longLivedTree.itemCheck())
}
