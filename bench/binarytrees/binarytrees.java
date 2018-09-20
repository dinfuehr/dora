/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Jarkko Miettinen
 * modified by Daryl Griffith
 * *reset*
*/

public class binarytrees {

    public static void main(String[] args) {
        int maxDepth;

        {
            int n = 0;

            if (args.length > 0) {
                n = Integer.parseInt(args[0]);
            }
            maxDepth = (6 > n) ? 6 : n;
        }
        {
            int stretchDepth = maxDepth + 1;

            System.out.println("stretch tree of depth " + stretchDepth +
		            "\t check: " + checkTree(createTree(stretchDepth)));
        }
        trees(maxDepth);
    }

    public static void trees(int maxDepth) {
        TreeNode longLastingNode = createTree(maxDepth);
        int depth = 4;

        do {
            int iterations = 16 << (maxDepth - depth);

            loops(iterations, depth);
            depth += 2;
        } while (depth <= maxDepth);

        System.out.println("long lived tree of depth " + maxDepth
		      + "\t check: " + checkTree(longLastingNode));
    }

    public static void loops(int iterations, int depth) {
        int check = 0;
        int item = 0;

        do {
            check += checkTree(createTree(depth));
            item++;
        } while (item < iterations);
        System.out.println(iterations + "\t trees of depth " +
		depth + "\t check: " + check);
    }

    public static TreeNode createTree(int depth) {
        TreeNode node = new TreeNode();

        if (depth > 0) {
            depth--;
            node.left = createTree(depth);
            node.right = createTree(depth);
        }
        return node;
    }

    public static int checkTree(TreeNode node) {
        if (node.left == null) {
            return 1;
        }
        return checkTree(node.left) + checkTree(node.right) + 1;
    }

    public static class TreeNode {

        private int item;
        private TreeNode left, right;
    }
}
