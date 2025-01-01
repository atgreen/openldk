// Tags: JDK1.1

/* Copyright (c) 2005 by Free Software Foundation, Inc.

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

package gnu.testlet.java.io.ObjectOutputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectStreamConstants;
import java.io.IOException;
import java.io.ByteArrayOutputStream;
import java.io.ByteArrayInputStream;

/* Basic tests for ObjectOutputStream compliance with the serialization data
   stream specification. */
public class StreamDataTest implements Testlet
{
  static int offset = 0;
  static byte[] streamData;

  static boolean compare(int[] expectedData)
  {
    try
    {
      for (int i=0; i < expectedData.length; i++)
	if (streamData[offset + i] != (byte) (expectedData[i] & 0xff))
	  return false;
    }
    finally
    {
      offset += expectedData.length;
    }
    
    return true;
  }

  public void test(TestHarness harness)
  {
    try
    {
      checkStream(harness);
    }
    catch (IOException x)
    {
      harness.fail(x.toString());
    }
  }

  public void checkStream(TestHarness harness) throws IOException
  {
    ByteArrayOutputStream os = new ByteArrayOutputStream();
    ObjectOutputStream oos = new ObjectOutputStream(os);
    
    oos.useProtocolVersion(ObjectStreamConstants.PROTOCOL_VERSION_2);
    
    oos.writeInt(1);
    oos.writeShort((short) 7);
    oos.writeFloat(9.96601f);
    oos.writeLong(-900000000000001l);
    oos.writeShort((short) -1);
    oos.writeDouble(Math.PI);
    oos.writeByte((byte) 'z');
    oos.writeDouble(Double.NaN);
    
    byte[] bytes = new byte[] {-1,2,-3,4,-5};
    oos.writeObject(bytes);
    oos.writeByte(100);
    oos.writeChar('X');
    oos.close();

    streamData = os.toByteArray();
    
    harness.check(streamData.length, 76, "Stream length");

    int[] data;
    data = new int[] {0xac, 0xed};
    harness.check(compare(data), "magic");
    data = new int[] {0x0, 0x5};
    harness.check(compare(data), "version");
    data = new int[] {0x77, 0x25};
    harness.check(compare(data), "TC_BLOCKDATA");
    data = new int[] {0x0, 0x0, 0x0, 0x1};
    harness.check(compare(data), "(int) 1");
    data = new int[] {0x0, 0x7};
    harness.check(compare(data), "(short) 7");
    data = new int[] {0x41, 0x1f, 0x74, 0xc7};
    harness.check(compare(data), "(float)");
    data = new int[] {0xff, 0xfc, 0xcd, 0x74, 0x6b, 0xb3, 0xbf, 0xff};
    harness.check(compare(data), "(long)");
    data = new int[] {0xff, 0xff};
    harness.check(compare(data), "(short) -1");
    data = new int[] {0x40, 0x9, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18};
    harness.check(compare(data), "(double) Math.PI");
    data = new int[] {0x7a};
    harness.check(compare(data), "(byte) 'z'");
    data = new int[] {0x7f, 0xf8, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0};
    harness.check(compare(data), "(double) Double.NaN");
    data = new int[] {0x75};
    harness.check(compare(data), "TC_NEWARRAY");
    data = new int[] {0x72};
    harness.check(compare(data), "TC_CLASSDESC");
    data = new int[] {0x0, 0x2, 0x5b, 0x42};
    harness.check(compare(data), "[B");
    data = new int[] {0xac, 0xf3, 0x17, 0xf8, 0x6, 0x8, 0x54, 0xe0};
    harness.check(compare(data), "SerialVersionUID");
    data = new int[] {0x2, 0x0, 0x0, 0x78};
    harness.check(compare(data), "Handle");
    data = new int[] {0x70};
    harness.check(compare(data), "ClassDescInfo");
    data = new int[] {0x0, 0x0, 0x0, 0x5};
    harness.check(compare(data), "array size (int) 5");
    data = new int[] {0xff, 0x2, 0xfd, 0x4, 0xfb};
    harness.check(compare(data), "int[] array data");
    data = new int[] {0x77, 0x3};
    harness.check(compare(data), "TC_BLOCKDATA");
    data = new int[] {0x64};
    harness.check(compare(data), "(byte) 100");
    data = new int[] {0x0, 0x58};
    harness.check(compare(data), "(char) 'X'");
  }
}
