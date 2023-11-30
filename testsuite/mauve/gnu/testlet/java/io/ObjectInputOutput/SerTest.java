// Tags: JDK1.2
// Uses: SerBase

/* SerTest.java -- Test class that "overrides" private field 'a'.

   Copyright (c) 2002 by Free Software Foundation, Inc.
   Written by Mark Wielaard (mark@klomp.org).
   Based on a test by Jeroen Frijters (jeroen@sumatra.nl).

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation, version 2. (see COPYING)

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation
   Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA */

package gnu.testlet.java.io.ObjectInputOutput;

import java.io.*;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class SerTest extends SerBase implements Testlet
{
  // This is THE field (this shadows the a field in the super class).
  private int a;

  public SerTest()
  {
    this(1,2);
  }

  SerTest(int a1, int a2)
  {
    super(a2);
    a = a1;
  }

  public void test(TestHarness harness)
  {
    try
      {
	SerTest original = this;
	ByteArrayOutputStream baos = new ByteArrayOutputStream();
	new ObjectOutputStream(baos).writeObject(original);
	ByteArrayInputStream bais
		= new ByteArrayInputStream(baos.toByteArray());
	SerTest serialized = (SerTest) new ObjectInputStream(bais).readObject();

	harness.check(serialized.a, original.a);
	harness.check(serialized.getA(), original.getA());
      }
    catch (Throwable t)
      {
	harness.check(false);
	harness.debug(t);
      }
  }
}
