/*
 * Copyright (c) 2001 Sun Microsystems, Inc. 
 * 
 * See the file "license.terms" for information on usage and redistribution 
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

/*
 This benchmark was originally developed by Dave Detlefs to exercise
 large old-generation heaps during research on concurrent
 collection.  Matthias Jacob found some bugs, and Will Clinger did
 an extensive rewrite (with several bug fixes), leading to the 1.0
 version.
*/

/*
 * Major revision history since 1.0:
 */

import java.text.*;
import java.util.Random;

class TreeNode {
    public TreeNode left, right;
    public int val;                // will always be the height of the tree
}


/* Args:
   live-data-size: in megabytes (approximate, will be rounded down).
   work: units of mutator non-allocation work per byte allocated,
     (in unspecified units.  This will affect the promotion rate
      printed at the end of the run: more mutator work per step implies
      fewer steps per second implies fewer bytes promoted per second.)
   short/long ratio: ratio of short-lived bytes allocated to long-lived
      bytes allocated.
   pointer mutation rate: number of pointer mutations per step.
   steps: number of steps to do.
*/
   
public class gcold {

  // Command-line parameters.

  private static int size, workUnits, promoteRate, ptrMutRate, steps;

  // Constants.

  private static final int MEG = 1000000;
  private static final int INSIGNIFICANT = 999; // this many bytes don't matter
  private static final int BYTES_PER_WORD = 8;
  private static final int BYTES_PER_NODE = 40; // bytes per TreeNode
  private static final int WORDS_DEAD = 100;    // size of young garbage object

  private final static int treeHeight = 14;
  private final static long treeSize = heightToBytes(treeHeight);

  private static final String msg1
    = "Usage: java GCOld <size> <work> <ratio> <mutation> <steps>";
  private static final String msg2
    = "  where <size> is the live storage in megabytes";
  private static final String msg3
    = "        <work> is the mutator work per step (arbitrary units)";
  private static final String msg4
    = "        <ratio> is the ratio of short-lived to long-lived allocation";
  private static final String msg5
    = "        <mutation> is the mutations per step";
  private static final String msg6
    = "        <steps> is the number of steps";

  // Counters (and global variables that discourage optimization)

  private static long youngBytes = 0;    // total young bytes allocated
  private static long nodes = 0;         // total tree nodes allocated
  private static long actuallyMut = 0;   // pointer mutations in old trees
  private static long mutatorSum = 0;    // checksum to discourage optimization
  public static int[] aexport;           // exported array to discourage opt

  // Global variables.

  private static TreeNode[] trees;
  private static int where = 0;               // roving index into trees
  private static Random rnd = new Random();

  // Returns the height of the given tree.

  private static int height (TreeNode t) {
    if (t == null) {
      return 0;
    }
    else {
      return 1 + Math.max (height (t.left), height (t.right));
    }
  }

  // Returns the length of the shortest path in the given tree.

  private static int shortestPath (TreeNode t) {
    if (t == null) {
      return 0;
    }
    else {
      return 1 + Math.min (shortestPath (t.left), shortestPath (t.right));
    }
  }

  // Returns the number of nodes in a balanced tree of the given height.

  private static long heightToNodes (int h) {
    if (h == 0) {
      return 0;
    }
    else {
      long n = 1;
      while (h > 1) {
        n = n + n;
        h = h - 1;
      }
      return n + n - 1;
    }
  }

  // Returns the number of bytes in a balanced tree of the given height.

  private static long heightToBytes (int h) {
    return BYTES_PER_NODE * heightToNodes (h);
  }

  // Returns the height of the largest balanced tree
  // that has no more than the given number of nodes.

  private static int nodesToHeight (long nodes) {
    int h = 1;
    long n = 1;
    while (n + n - 1 <= nodes) {
      n = n + n;
      h = h + 1;
    }
    return h - 1;
  }

  // Returns the height of the largest balanced tree
  // that occupies no more than the given number of bytes.

  private static int bytesToHeight (long bytes) {
    return nodesToHeight (bytes / BYTES_PER_NODE);
  }

  // Returns a newly allocated balanced binary tree of height h.

  private static TreeNode makeTree(int h) {
    if (h == 0) return null;
    else {
      TreeNode res = new TreeNode();
      nodes++;
      res.left = makeTree(h-1);
      res.right = makeTree(h-1);
      res.val = h;
      return res;
    }
  }

  // Allocates approximately size megabytes of trees and stores
  // them into a global array.

  private static void init() {
    System.out.println("treeSize = " + treeSize);
    int ntrees = (int) ((size * MEG) / treeSize);
    trees = new TreeNode[ntrees];

    System.err.println("Allocating " + ntrees + " trees.");
    System.err.println("  (" + (ntrees * treeSize) + " bytes)");
    for (int i = 0; i < ntrees; i++) {
      trees[i] = makeTree(treeHeight);
    }
    System.err.println("  (" + nodes + " nodes)");
  }

  // Confirms that all trees are balanced and have the correct height.

  private static void checkTrees() {
    int ntrees = trees.length;
    for (int i = 0; i < ntrees; i++) {
      TreeNode t = trees[i];
      int h1 = height(t);
      int h2 = shortestPath(t);
      if ((h1 != treeHeight) || (h2 != treeHeight)) {
        System.err.println("*****BUG: " + h1 + " " + h2);
      }
    }
  }

  // Called only by replaceTree (below) and by itself.

  private static void replaceTreeWork(TreeNode full, TreeNode partial,
				      boolean dir) {
    boolean canGoLeft = full.left != null && full.left.val > partial.val;
    boolean canGoRight = full.right != null && full.right.val > partial.val;
    if (canGoLeft && canGoRight) {
      if (dir)
	replaceTreeWork(full.left, partial, !dir);
      else
	replaceTreeWork(full.right, partial, !dir);
    } else if (!canGoLeft && !canGoRight) {
      if (dir)
	full.left = partial;
      else
	full.right = partial;
    } else if (!canGoLeft) {
      full.left = partial;
    } else {
      full.right = partial;
    }
  }

  // Given a balanced tree full and a smaller balanced tree partial,
  // replaces an appropriate subtree of full by partial, taking care
  // to preserve the shape of the full tree.

  private static void replaceTree(TreeNode full, TreeNode partial) {
    boolean dir = (partial.val % 2) == 0;
    actuallyMut++;
    replaceTreeWork(full, partial, dir);
  }

  // Allocates approximately n bytes of long-lived storage,
  // replacing oldest existing long-lived storage.

  private static void oldGenAlloc(long n) {
    int full = (int) (n / treeSize);
    long partial = n % treeSize;
    // System.out.println("In oldGenAlloc, doing " + full + " full trees "
    // + "and one partial tree of size " + partial);
    for (int i = 0; i < full; i++) {
      trees[where++] = makeTree(treeHeight);
      if (where == trees.length) where = 0;
    }
    while (partial > INSIGNIFICANT) {
      int h = bytesToHeight(partial);
      TreeNode newTree = makeTree(h);
      replaceTree(trees[where++], newTree);
      if (where == trees.length) where = 0;
      partial = partial - heightToBytes(h);
    }
  }

  // Interchanges two randomly selected subtrees (of same size and depth).

  private static void oldGenSwapSubtrees() {
    // Randomly pick:
    //   * two tree indices 
    //   * A depth
    //   * A path to that depth.
    int index1 = rnd.nextInt(trees.length);
    int index2 = rnd.nextInt(trees.length);
    int depth = rnd.nextInt(treeHeight);
    int path = rnd.nextInt();
    TreeNode tn1 = trees[index1];
    TreeNode tn2 = trees[index2];
    for (int i = 0; i < depth; i++) {
      if ((path & 1) == 0) {
	tn1 = tn1.left;
	tn2 = tn2.left;
      } else {
	tn1 = tn1.right;
	tn2 = tn2.right;
      }
      path >>= 1;
    }
    TreeNode tmp;
    if ((path & 1) == 0) {
      tmp = tn1.left;
      tn1.left = tn2.left;
      tn2.left = tmp;
    } else {
      tmp = tn1.right;
      tn1.right = tn2.right;
      tn2.right = tmp;
    }
    actuallyMut += 2;
  }

  // Update "n" old-generation pointers.

  private static void oldGenMut(long n) {
    for (int i = 0; i < n/2; i++) {
      oldGenSwapSubtrees();
    }
  }

  // Does the amount of mutator work appropriate for n bytes of young-gen
  // garbage allocation.

  private static void doMutWork(long n) {
    int sum = 0;
    long limit = workUnits*n/10;
    for (long k = 0; k < limit; k++) sum++;
    // We don't want dead code elimination to eliminate the loop above.
    mutatorSum = mutatorSum + sum;
  }

  // Allocate n bytes of young-gen garbage, in units of "nwords"
  // words.

  private static void doYoungGenAlloc(long n, int nwords) {
    final int nbytes = nwords*BYTES_PER_WORD;
    int allocated = 0;
    while (allocated < n) {
      aexport = new int[nwords];
      /* System.err.println("Step"); */
      allocated += nbytes;
    }
    youngBytes = youngBytes + allocated;
  }

  // Allocate "n" bytes of young-gen data; and do the
  // corresponding amount of old-gen allocation and pointer
  // mutation.

  // oldGenAlloc may perform some mutations, so this code
  // takes those mutations into account.

  private static void doStep(long n) {
    long mutations = actuallyMut;

    doYoungGenAlloc(n, WORDS_DEAD);
    doMutWork(n);
    oldGenAlloc(n / promoteRate); 
    oldGenMut(Math.max(0L, (mutations + ptrMutRate) - actuallyMut));
  }

  private static String version = "1.0";

  public static void main(String[] args) {
    if (args.length != 5) {
      System.err.println(msg1);
      System.err.println(msg2);
      System.err.println(msg3);
      System.err.println(msg4);
      System.err.println(msg5);
      System.err.println(msg6);
      return;
    }

    size = Integer.parseInt(args[0]);
    workUnits = Integer.parseInt(args[1]);
    promoteRate = Integer.parseInt(args[2]);
    ptrMutRate = Integer.parseInt(args[3]);
    steps = Integer.parseInt(args[4]);

    System.out.println("GCOld: version " + version);
    System.out.println(size + " megabytes of live storage");
    System.out.println(workUnits + " work units per step");
    System.out.println("promotion ratio is 1:" + promoteRate);
    System.out.println("pointer mutation rate is " + ptrMutRate);
    System.out.println(steps + " steps");

    init();
//  checkTrees();
    youngBytes = 0;
    nodes = 0;

    System.err.println("Initialization complete...");

    long start = System.currentTimeMillis();

    for (int step = 0; step < steps; step++) {
      doStep(MEG);
    }

    long end = System.currentTimeMillis();
    float secs = ((float)(end-start))/1000.0F;

//  checkTrees();

    NumberFormat nf = NumberFormat.getInstance();
    nf.setMaximumFractionDigits(1);
    System.out.println("\nTook " + nf.format(secs) + " sec in steady state.");
    nf.setMaximumFractionDigits(2);
    System.out.println("Allocated " + steps + " Mb of young gen garbage"
		       + " (= " + nf.format(((float)steps)/secs) +
		       " Mb/sec)");
    System.out.println("    (actually allocated " +
                       nf.format(((float) youngBytes)/MEG) + " megabytes)");
    float promoted = ((float)steps) / (float)promoteRate;
    System.out.println("Promoted " + promoted +
		       " Mb (= " + nf.format(promoted/secs) + " Mb/sec)");
    System.out.println("    (actually promoted " +
                       nf.format(((float) (nodes * BYTES_PER_NODE))/MEG) +
                       " megabytes)");
    if (ptrMutRate != 0) {
      System.out.println("Mutated " + actuallyMut +
			 " pointers (= " +
                         nf.format(actuallyMut/secs) + " ptrs/sec)");

    }
    // This output serves mainly to discourage optimization.
    System.out.println("Checksum = " + (mutatorSum + aexport.length));

  }
}
