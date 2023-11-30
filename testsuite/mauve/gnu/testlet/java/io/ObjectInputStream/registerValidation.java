// Tags: JDK1.1
// Uses: TestObjectInputValidation

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

package gnu.testlet.java.io.ObjectInputStream;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectInputValidation;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;

/**
 * Some checks for registerValidation() method of the {@link ObjectInputStream} class.
 */
public class registerValidation implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    TestObjectInputValidation t1 = new TestObjectInputValidation("Name1");
    TestObjectInputValidation t2 = null;

    try {
      ByteArrayOutputStream buffer = new ByteArrayOutputStream();
      ObjectOutput out = new ObjectOutputStream(buffer);
      out.writeObject(t1);
      out.close();

      ObjectInput in = new ObjectInputStream(
        new ByteArrayInputStream(buffer.toByteArray())
      );
      t2 = (TestObjectInputValidation) in.readObject();
      in.close();

      harness.check(t2, t1); // name and priority the same
      harness.check(t2.object, t2); // has self-reference
      harness.check(t2.validated != null);

      Object[] ps = t2.validated.toArray();
      int[] priorities = new int[ps.length];
      for (int i = 0; i < ps.length; i++)
	priorities[i] = ((Integer) ps[i]).intValue();
      harness.check(priorities != null);
      harness.check(priorities.length, 5);
      harness.check(priorities[0], -12);
      harness.check(priorities[1], -10);
      harness.check(priorities[2], 10);
      harness.check(priorities[3], 11);
      harness.check(priorities[4], 10); // The priority 12 "this" again.
    }
    catch (Exception e) {
      harness.debug(e);
      harness.check(false, e.toString());
    }
  }
  
}
