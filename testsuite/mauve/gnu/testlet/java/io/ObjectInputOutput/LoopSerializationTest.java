/* LoopSerializationTest.java -- Test back references in serialization
 Copyright (C) 2006 Olivier Jolly <olivier.jolly@pcedev.com>
 This file is part of Mauve.

 Mauve is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2, or (at your option)
 any later version.

 Mauve is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with Mauve; see the file COPYING.  If not, write to the
 Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 02110-1301 USA.

 */

// Tags: JDK1.2
// Uses: SerializableLoopA SerializableLoopB


package gnu.testlet.java.io.ObjectInputOutput;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.StreamCorruptedException;

public class LoopSerializationTest implements Testlet
{

  public void test(TestHarness harness)
  {

    SerializableLoopA a = new SerializableLoopA();
    SerializableLoopB b = new SerializableLoopB();

    a.setB(b);
    b.setA(a);

    harness.checkPoint("LoopSerializationTest");

    harness.check(a.getB(), b);
    harness.check(b.getA(), a);

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    try
      {
        new ObjectOutputStream(baos).writeObject(a);
      }
    catch (IOException e)
      {
        harness.debug(e);
        harness.fail("Serialiazing a loop");
      }
    ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
    try
      {
        SerializableLoopA serialized = (SerializableLoopA) new ObjectInputStream(
                                                                                 bais).readObject();
        harness.check(serialized.getB(), b);
      }
    catch (StreamCorruptedException e)
      {
        harness.debug(e);
        harness.fail("Deserialiazing a loop");
      }
    catch (ClassNotFoundException e)
      {
        harness.debug(e);
        harness.fail("Deserialiazing a loop");
      }
    catch (IOException e)
      {
        harness.debug(e);
        harness.fail("Deserialiazing a loop");
      }
  }
}
