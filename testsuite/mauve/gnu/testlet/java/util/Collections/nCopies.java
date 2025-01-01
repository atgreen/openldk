// Tags: JDK1.2

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.util.Collections;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.util.Collections;
import java.util.List;

public class nCopies implements Testlet
{

  public void test(TestHarness harness) 
  {
    // try n = 0
    List list = Collections.nCopies(0, "Y");
    harness.check(list.isEmpty());

    // try n > 0
    list = Collections.nCopies(10, "X");
    harness.check(list.size() == 10);
    harness.check(list.get(0).equals("X"));
    harness.check(list.get(9).equals("X"));
    
    // try n < 0
    boolean pass = false;
    try
    {
      list = Collections.nCopies(-1, "X");
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // try null object
    list = Collections.nCopies(3, null);
    harness.check(list.size() == 3);
    harness.check(list.get(0) == null);
    harness.check(list.get(1) == null);
    harness.check(list.get(2) == null);

    // confirm list is unmodifiable
    list = Collections.nCopies(10, "Y");
    pass = false;
    try
    {
      list.add("Z");
    }
    catch (UnsupportedOperationException e)
    {
      pass = true;
    }
    harness.check(pass);
 
    // the method should return a Serializable list
    testSerialization(harness);
  }
  
  private void testSerialization(TestHarness harness) 
  {
    List list1 = Collections.nCopies(99, "X");
    List list2 = null;

    try {
      ByteArrayOutputStream buffer = new ByteArrayOutputStream();
      ObjectOutput out = new ObjectOutputStream(buffer);
      out.writeObject(list1);
      out.close();

      ObjectInput in = new ObjectInputStream(
        new ByteArrayInputStream(buffer.toByteArray())
      );
      list2 = (List) in.readObject();
      in.close();
    }
    catch (Exception e) {
        harness.debug(e);
    }
    harness.check(list1.equals(list2));
  }
}
