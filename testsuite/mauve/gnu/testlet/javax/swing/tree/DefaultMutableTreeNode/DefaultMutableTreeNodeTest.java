// Tags: JDK1.2

// Copyright (C) 2004  Michael Koch <konqueror@gmx.de>

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.tree.DefaultMutableTreeNode;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.*;

import javax.swing.tree.*;

public class DefaultMutableTreeNodeTest implements Testlet
{
  public static DefaultMutableTreeNode A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z;
  public static Vector nodes = new Vector();

  static
  {
    A = new DefaultMutableTreeNode("A");
    B = new DefaultMutableTreeNode("B");
    C = new DefaultMutableTreeNode("C");
    D = new DefaultMutableTreeNode("D");
    E = new DefaultMutableTreeNode("E");
    F = new DefaultMutableTreeNode("F");
    G = new DefaultMutableTreeNode("G");
    H = new DefaultMutableTreeNode("H");
    I = new DefaultMutableTreeNode("I");
    J = new DefaultMutableTreeNode("J");
    K = new DefaultMutableTreeNode("K", false);
    L = new DefaultMutableTreeNode("L");
    M = new DefaultMutableTreeNode("M");
    N = new DefaultMutableTreeNode("N");
    O = new DefaultMutableTreeNode("O");
    P = new DefaultMutableTreeNode("P", false);
    Q = new DefaultMutableTreeNode("Q");
    R = new DefaultMutableTreeNode("R");
    S = new DefaultMutableTreeNode("S");
    T = new DefaultMutableTreeNode("T", false);
    U = new DefaultMutableTreeNode("U");
    V = new DefaultMutableTreeNode("V");
    W = new DefaultMutableTreeNode("W");
    X = new DefaultMutableTreeNode("X", false);
    Y = new DefaultMutableTreeNode("Y");
    Z = new DefaultMutableTreeNode("Z");

    A.add(B);
    A.add(C);
    A.add(D);
    B.add(E);
    B.add(F);
    C.add(G);
    D.add(H);
    D.add(I);
    E.add(J);
    F.add(K);
    G.add(L);
    G.add(M);
    H.add(N);
    I.add(O);
    I.add(P);
    J.add(Q);
    J.add(R);
    L.add(S);
    M.add(T);
    O.add(U);
    O.add(V);
    S.add(W);
    S.add(X);
    U.add(Y);
    Y.add(Z);

    nodes.add(A);
    nodes.add(B);
    nodes.add(C);
    nodes.add(D);
    nodes.add(E);
    nodes.add(F);
    nodes.add(G);
    nodes.add(H);
    nodes.add(I);
    nodes.add(J);
    nodes.add(K);
    nodes.add(L);
    nodes.add(M);
    nodes.add(N);
    nodes.add(O);
    nodes.add(P);
    nodes.add(Q);
    nodes.add(R);
    nodes.add(S);
    nodes.add(T);
    nodes.add(U);
    nodes.add(V);
    nodes.add(W);
    nodes.add(X);
    nodes.add(Y);
    nodes.add(Z);
  }

  public static void checkEnumeration(TestHarness h, Enumeration e, DefaultMutableTreeNode node)
  {
    if (node == null)
      {
        h.check(! e.hasMoreElements(), "enumeration has more elements");
    
        boolean ok = false;
        
        try
          {
            e.nextElement();
          }
        catch (NoSuchElementException ex)
          {
            ok = true;
          }

        h.check(ok, "throws NoSuchElementException");
      }
    else
      {
        h.check(e.hasMoreElements(), "enumeration has more elements");
        
        DefaultMutableTreeNode enode = (DefaultMutableTreeNode) e.nextElement();
        
        h.check(enode, node, "correct node as next element");
        h.debug("expected: " + ((String) node.getUserObject()));
        h.debug("got     : " + ((String) enode.getUserObject()));
      }
  }

  private void checkRoot(TestHarness h, DefaultMutableTreeNode node, boolean isRoot)
  {
    h.check(node.isRoot() == isRoot, "node is root");
  }

  private void checkIsLeaf(TestHarness h, DefaultMutableTreeNode node, boolean isLeaf)
  {
    h.check(node.isLeaf() == isLeaf, "isLeaf");
  }
  
  public void test(TestHarness h)
  {
    checkRoot(h, A,  true);
    checkRoot(h, B, false);
    checkRoot(h, C, false);
    checkRoot(h, D, false);
    checkRoot(h, E, false);
    checkRoot(h, F, false);
    checkRoot(h, G, false);
    checkRoot(h, H, false);
    checkRoot(h, I, false);
    checkRoot(h, J, false);
    checkRoot(h, K, false);
    checkRoot(h, L, false);
    checkRoot(h, M, false);
    checkRoot(h, N, false);
    checkRoot(h, O, false);
    checkRoot(h, P, false);
    checkRoot(h, Q, false);
    checkRoot(h, R, false);
    checkRoot(h, S, false);
    checkRoot(h, T, false);
    checkRoot(h, U, false);
    checkRoot(h, V, false);
    checkRoot(h, W, false);
    checkRoot(h, X, false);
    checkRoot(h, Y, false);
    checkRoot(h, Z, false);

    checkIsLeaf(h, A, false);
    checkIsLeaf(h, B, false);
    checkIsLeaf(h, C, false);
    checkIsLeaf(h, D, false);
    checkIsLeaf(h, E, false);
    checkIsLeaf(h, F, false);
    checkIsLeaf(h, G, false);
    checkIsLeaf(h, H, false);
    checkIsLeaf(h, I, false);
    checkIsLeaf(h, J, false);
    checkIsLeaf(h, K, true);
    checkIsLeaf(h, L, false);
    checkIsLeaf(h, M, false);
    checkIsLeaf(h, N, true);
    checkIsLeaf(h, O, false);
    checkIsLeaf(h, P, true);
    checkIsLeaf(h, Q, true);
    checkIsLeaf(h, R, true);
    checkIsLeaf(h, S, false);
    checkIsLeaf(h, T, true);
    checkIsLeaf(h, U, false);
    checkIsLeaf(h, V, true);
    checkIsLeaf(h, W, true);
    checkIsLeaf(h, X, true);
    checkIsLeaf(h, Y, false);
    checkIsLeaf(h, Z, true);
  }
}