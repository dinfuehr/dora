/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Léo Sarrazin 
*/

const TreeNode = function(left, right) {
    this.left  = left;
    this.right = right;
};

const itemCheck = function(node){
    if (node.left===null) return 1;
    return 1 + itemCheck(node.left) + itemCheck(node.right);
};

function bottomUpTree(depth){
    return depth>0 ? new TreeNode(
            bottomUpTree(depth-1),
            bottomUpTree(depth-1)
    ) : new TreeNode(null, null);
};

const n = process.argv.length > 2 ? parseInt(process.argv[2]) : 0;
const maxDepth = Math.max(6, n);
const stretchDepth = maxDepth + 1;

let check = itemCheck(bottomUpTree(stretchDepth));
console.log("stretch tree of depth "+ stretchDepth+ "\t check: "+ check);

const longLivedTree = bottomUpTree(maxDepth);

for (let depth=4; depth<=maxDepth; depth+=2){
    const iterations = 1 << maxDepth - depth + 4;
    check = 0;
    for (let i=1; i<=iterations; i++){
        check += itemCheck(bottomUpTree(depth));
    }
    console.log(iterations+"\t trees of depth "+ depth +"\t check: " + check);
}

console.log("long lived tree of depth "+ maxDepth
            + "\t check: "+ itemCheck(longLivedTree));
