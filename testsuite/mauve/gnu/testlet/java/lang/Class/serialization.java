// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

// This file is part of Mauve.

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.lang.Class;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Graphics2D;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Arc2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.io.Serializable;
import java.lang.reflect.Array;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Vector;
import java.util.Set;
import java.util.SortedSet;

import javax.swing.table.DefaultTableModel;

/**
 * Some checks for serialization of a Class instance.
 */
public class serialization implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testClass(Cloneable.class, harness);
    testClass(Comparable.class, harness);
    testClass(Serializable.class, harness);
    testClass(Externalizable.class, harness);
    testClass(String.class, harness);
    testClass(Number.class, harness);
    testClass(Boolean.class, harness);
    testClass(Integer.class, harness);
    testClass(Float.class, harness);
    testClass(Double.class, harness);
    testClass(Vector.class, harness);
    testClass(ArrayList.class, harness);
    testClass(DateFormat.class, harness);
    testClass(Point.class, harness);
    testClass(Rectangle.class, harness);
    testClass(Rectangle2D.class, harness);
    testClass(Rectangle2D.Double.class, harness);
    testClass(Line2D.class, harness);
    testClass(Arc2D.class, harness);
    testClass(RoundRectangle2D.class, harness);
    testClass(Graphics2D.class, harness);
    testClass(DefaultTableModel.class, harness);
    testClass(LayoutManager.class, harness);
    testClass(Array.class, harness);
    testClass(Object.class, harness);
    testClass(Class.class, harness);
    testClass(Throwable.class, harness);
    testClass(IOException.class, harness);
    testClass(Void.class, harness);
    testClass(ObjectStreamClass.class, harness);
    testClass(Collection.class, harness);
    testClass(Set.class, harness);
    testClass(SortedSet.class, harness);

    testClass(boolean.class, harness);
    testClass(byte.class, harness);
    testClass(short.class, harness);
    testClass(char.class, harness);
    testClass(int.class, harness);
    testClass(long.class, harness);
    testClass(float.class, harness);
    testClass(double.class, harness);
    testClass(void.class, harness);
  }
  
  private void testClass(Class c1, TestHarness harness) 
  {
    Class c2 = null;
    try {
      ByteArrayOutputStream buffer = new ByteArrayOutputStream();
      ObjectOutput out = new ObjectOutputStream(buffer);
      out.writeObject(c1);
      out.close();

      ObjectInput in = new ObjectInputStream(
        new ByteArrayInputStream(buffer.toByteArray())
      );
      c2 = (Class) in.readObject();
      in.close();
    }
    catch (Exception e) {
      harness.debug(e);
    }
    harness.check(c1.equals(c2), c1.toString());
  }
  
}
