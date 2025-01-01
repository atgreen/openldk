// Not a test, required by RMI_IIOP.java.

// Copyright (C) 2005 Audrius Meskauskas (AudriusA@Bioinformatics.org)

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.rmi.CORBA.Tie;

import java.io.Serializable;

/**
 * This file is part of the CORBA RMI over IIOP the test executable
 * class being gnu.testlet.javax.rmi.CORBA.Tie.RMI_IIOP. It is a
 * node of the graph that must be flattened and passed via RMI-IIOP.
 *
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public class NodeObject
  implements Serializable
{
  /**
   * The label facilitates orientation.
   */
  public String label;

  /**
   * The pointer for forming the network.
   */
  public NodeObject a;

  /**
   * Another pointer to form a trees.
   */
  public NodeObject b;
  
  /**
   * Another RMI_test.
   */
  RMI_test z_anotherTest;
  
  /**
   * An array of "another RMI tests".
   */
  RMI_test[] anotherTestArray;
  
  RMI_test[][][] ku;

  /**
   * Some transient field.
   */
  transient Object transientField;

  /**
   * Some static field.
   */
  static Object staticField;

  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 0x7;

  public NodeObject(String a_label)
  {
    label = a_label;
  }

  public NodeObject()
  {
    this("<no label>");
  }

  public String toString(int chain)
  {
    if (chain > 7)
      return "...";
    StringBuffer sb = new StringBuffer();

    sb.append(label);

    if (b != null)
      sb.append("(" + b + ")");

    sb.append(":");
    if (a != null)
      sb.append(a.toString(chain + 1));
    else
      sb.append("null");

    return sb.toString();
  }

  public String toString()
  {
    return toString(0);
  }

  public static NodeObject create1()
  {
    NodeObject a = new NodeObject("a");
    NodeObject b = new NodeObject("b");
    NodeObject c = new NodeObject("c");

    NodeObject d = new NodeObject("d");
    NodeObject e = new NodeObject("e");
    NodeObject f = new NodeObject("f");

    // Lock f on self.
    f.a = f;

    // Form a digraph.
    d.a = e;
    e.a = d;

    e.b = f;

    // Form a triangle.
    a.a = b;
    b.a = c;
    c.a = a;

    // Add D to a and c.
    a.b = d;
    c.b = d;

    return a;
  }

  /**
   * Create a closed ring.
   */
  public static NodeObject create2()
  {
    NodeObject a = new NodeObject("a");
    NodeObject b = new NodeObject("b");
    NodeObject c = new NodeObject("c");

    NodeObject d = new NodeObject("d");
    NodeObject e = new NodeObject("e");
    NodeObject f = new NodeObject("f");
    
    a.a = b;
    b.a = c;
    c.a = d;
    d.a = e;
    e.a = f;
    f.a = a;
    
    return a;
  }
  
  public static void main(String[] args)
  {
    System.out.println(create1());
    System.out.println(create2());    
  }

}
