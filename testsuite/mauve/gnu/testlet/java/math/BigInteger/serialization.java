// Tags: JDK1.1

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.math.BigInteger;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.math.BigInteger;

/**
 * Some checks for serialization of the {@link BigInteger} class.
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
    BigInteger b1 = new BigInteger("-4294967296");
    BigInteger b2 = null;

    try {
      ByteArrayOutputStream buffer = new ByteArrayOutputStream();
      ObjectOutput out = new ObjectOutputStream(buffer);
      out.writeObject(b1);
      out.close();

      ObjectInput in = new ObjectInputStream(
        new ByteArrayInputStream(buffer.toByteArray())
      );
      b2 = (BigInteger) in.readObject();
      in.close();
    }
    catch (Exception e) {
      harness.debug(e);
    }
    harness.check(b1.equals(b2));
    harness.check(b1.byteValue() == b2.byteValue());
    harness.check(b1.shortValue() == b2.shortValue());
    harness.check(b1.intValue() == b2.intValue());
    harness.check(b1.longValue() == b2.longValue());  // see bug parade 4823171
    harness.check(b1.floatValue() == b2.floatValue());
    harness.check(b1.doubleValue() == b2.doubleValue());
  }
  
}
