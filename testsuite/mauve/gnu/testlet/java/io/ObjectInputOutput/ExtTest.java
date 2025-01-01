// Tags: JDK1.2

/* ExtTest.java -- Regression test for GNU Classpath bug pertaining
   to the handling of block data.

   Copyright (c) 2004 by Free Software Foundation, Inc.
   Written by Jeroen Frijters (jeroen@frijters.net).

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

public class ExtTest implements Testlet, Serializable
{
  public static class Inner implements Externalizable
  {
    public void readExternal(ObjectInput ois) {}
    public void writeExternal(ObjectOutput oos) {}
  }

  private Object ext = new Inner();
  private String test = "test";

  public void test(TestHarness harness)
  {
    try
      {
	ByteArrayOutputStream baos = new ByteArrayOutputStream();
	new ObjectOutputStream(baos).writeObject(this);
	ByteArrayInputStream bais
		= new ByteArrayInputStream(baos.toByteArray());
	ExtTest serialized = (ExtTest) new ObjectInputStream(bais).readObject();

	harness.check(serialized.ext.getClass(), this.ext.getClass());
	harness.check(serialized.test.equals(this.test));
      }
    catch (Throwable t)
      {
	harness.check(false);
	harness.debug(t);
      }
  }
}
