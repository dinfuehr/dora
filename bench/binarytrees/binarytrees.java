/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Jarkko Miettinen
 * modified by Daryl Griffith
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
		            "\t check: " + checkTree(createTree(0, stretchDepth)));
        }
        trees(maxDepth);
    }

    public static void trees(int maxDepth) {
        TreeNode longLastingNode = createTree(0, maxDepth);
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
            check += checkTree(createTree(item, depth)) +
		 checkTree(createTree(-item, depth));
            item++;
        } while (item < iterations);
        System.out.println((iterations << 1) + "\t trees of depth " +
		depth + "\t check: " + check);
    }

    public static TreeNode createTree(int item, int depth) {
        TreeNode node = new TreeNode();

        node.item = item;
        if (depth > 0) {
            item = item + item;
            depth--;
            node.left = createTree(item - 1, depth);
            node.right = createTree(item, depth);
        }
        return node;
    }

    public static int checkTree(TreeNode node) {
        if (node.left == null) {
            return node.item;
        }
        return checkTree(node.left) - checkTree(node.right) + node.item;
    }

    public static class TreeNode {

        private int item;
        private TreeNode left, right;
    }
}
