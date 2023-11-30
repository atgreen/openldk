// Tags: JDK1.2
// Uses: Test

/* Deserialize.java -- Tests class which are not deserializable.
 * Imported from Kaffe 1.1.4.
 * Adapted by Guilhem Lavaux <guilhem@kaffe.org>.
 *
 * This file is part of Mauve.
 *
 * Mauve is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * Mauve is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mauve; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

package gnu.testlet.java.io.ObjectInputOutput;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.Serializable;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamException;
import java.io.InvalidClassException;

public class Deserializable implements Testlet
{  
  // this class is not serializable as it does not have <init>()
  static class NotSerializable1 {
    public NotSerializable1(int dummy) {
    }
  }
  
  static class Serialized1
    extends NotSerializable1
    implements Serializable
  {
    public Serialized1(int i) {
      super(i);
    }
  }
  
  
  // this class is not serializable as <init>() is private
  static class NotSerializable2 {
    public NotSerializable2(int dummy) {
    }
    
    private NotSerializable2() {
    }
  }
  
  static class Serialized2
    extends NotSerializable2
    implements Serializable
  {
    static int count = 0;
    public int i;
    
    public Serialized2(int i) {
      super(i);
      this.i = i;
    }
  }
    
  public void testObject(TestHarness harness, Object a)
  {
    try
      {
	FileOutputStream fos = new FileOutputStream ("frozen_serial");
	ObjectOutputStream oos = new ObjectOutputStream (fos);
	oos.writeObject (a);
	oos.flush ();
      }
    catch (Exception e)
      {
	harness.fail("Unexpected exception " + e);
	harness.debug(e);	
      }

    harness.checkPoint("Deserialize " + a.getClass().getName());
    try
      {
	FileInputStream fis = new FileInputStream ("frozen_serial");
	ObjectInputStream ois = new ObjectInputStream (fis);

	Object b = ois.readObject ();
	
	harness.fail("Was expecting an InvalidClassException");
      }
    catch (InvalidClassException e)
      {
	harness.check(true);
	harness.debug(e);
      }
    catch (Exception e2)
      {
	harness.fail("Wrong exception");
	harness.debug(e2);
      }
  }

  public void test(TestHarness harness)
  {
    testObject(harness, new Serialized1(10));
    testObject(harness, new Serialized2(10));
  }
}
