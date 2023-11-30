// Tags: JDK1.4

// Copyright (C) 2006 Red Hat

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

package gnu.testlet.javax.imageio.stream.MemoryCacheImageInputStream;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.ByteOrder;
import java.util.Arrays;

import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.ImageInputStreamImpl;
import javax.imageio.stream.MemoryCacheImageInputStream;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Test ImageInputStreamImpl.
 */
public class TestImageInputStreamImpl
  implements Testlet
{

  public void test(TestHarness h)
  {
    int k = 0;

    try
      {
        // Test unsigned values.
        byte[] b = new byte[]
          { 
            (byte) 0x72,
            (byte) 0x70,
            (byte) 0x05,
            (byte) 0x77,
            (byte) 0xac,
            (byte) 0xf2,
            (byte) 0x3b,
            (byte) 0x67
          };

        ByteArrayInputStream bs = new ByteArrayInputStream(b);
        ImageInputStream i = new MemoryCacheImageInputStream(bs);

        // Test ByteOrder.BIG_ENDIAN, the default.

        h.check(i.getByteOrder() == ByteOrder.BIG_ENDIAN);

        h.check(i.read() == 114);
        i.seek(0);
        h.check(i.readBoolean() == true);
        i.seek(0);
        h.check(i.readByte() == 114);
        i.seek(0);
        h.check(i.readChar() == '\u7270');
        i.seek(0);
        h.check(Double.compare(i.readDouble(), 1.709290273164385E243) == 0);
        i.seek(0);
        h.check(Float.compare(i.readFloat(), 4.7541126E30f) == 0);
        i.seek(0);
        h.check(i.readInt() == 1919944055);
        i.seek(0);
        h.check(i.readLong() == 8246096929276181351L);
        i.seek(0);
        h.check(i.readShort() == 29296);
        i.seek(0);
        h.check(i.readUnsignedByte() == 114);
        i.seek(0);
        h.check(i.readUnsignedInt() == 1919944055);
        i.seek(0);
        h.check(i.readUnsignedShort() == 29296);

        // Test ByteOrder.LITTLE_ENDIAN.
        i.setByteOrder(ByteOrder.LITTLE_ENDIAN);

        h.check(i.getByteOrder() == ByteOrder.LITTLE_ENDIAN);
        i.seek(0);
        h.check(i.read() == 114);
        i.seek(0);
        h.check(i.readBoolean() == true);
        i.seek(0);
        h.check(i.readByte() == 114);
        i.seek(0);
        h.check(i.readChar() == '\u7072');
        i.seek(0);
        h.check(Double.compare(i.readDouble(), 1.9456609400629563E189) == 0);
        i.seek(0);
        h.check(Float.compare(i.readFloat(), 2.7064693E33f) == 0);
        i.seek(0);
        h.check(i.readInt() == 1996845170);
        i.seek(0);
        h.check(i.readLong() == 7438806032077647986L);
        i.seek(0);
        h.check(i.readShort() == 28786);
        i.seek(0);
        h.check(i.readUnsignedByte() == 114);
        i.seek(0);
        h.check(i.readUnsignedInt() == 1996845170);
        i.seek(0);
        h.check(i.readUnsignedShort() == 28786);

        // Test unsigned values.
        b = new byte[]
          { 
            (byte) 0x92,
            (byte) 0x80,
            (byte) 0x05,
            (byte) 0x77,
            (byte) 0xac,
            (byte) 0xf2,
            (byte) 0x8b,
            (byte) 0xa7
          };

        bs = new ByteArrayInputStream(b);
        i = new MemoryCacheImageInputStream(bs);

        // Test ByteOrder.BIG_ENDIAN, the default.
        h.check(i.read() == 146);
        i.seek(0);
        h.check(i.readBoolean() == true);
        i.seek(0);
        h.check(i.readByte() == -110);
        i.seek(0);
        h.check(i.readChar() == '\u9280');
        i.seek(0);
        h.check(Double.compare(i.readDouble(), -1.4183142849706364E-219) == 0);
        i.seek(0);
        h.check(Float.compare(i.readFloat(), -8.079283E-28f) == 0);
        i.seek(0);
        h.check(i.readInt() == -1837103753);
        i.seek(0);
        h.check(i.readLong() == -7890300535592285273L);
        i.seek(0);
        h.check(i.readShort() == -28032);
        i.seek(0);
        h.check(i.readUnsignedByte() == 146);
        i.seek(0);
        h.check(i.readUnsignedInt() == 2457863543L);
        i.seek(0);
        h.check(i.readUnsignedShort() == 37504);

        // Test ByteOrder.LITTLE_ENDIAN.
        i.setByteOrder(ByteOrder.LITTLE_ENDIAN);
        i.seek(0);
        h.check(i.read() == 146);
        i.seek(0);
        h.check(i.readBoolean() == true);
        i.seek(0);
        h.check(i.readByte() == -110);
        i.seek(0);
        h.check(i.readChar() == '\u8092');
        i.seek(0);
        h.check(Double.compare(i.readDouble(), -3.463391436203922E-118) == 0);
        i.seek(0);
        h.check(Float.compare(i.readFloat(), 2.707747E33f) == 0);
        i.seek(0);
        h.check(i.readInt() == 1996849298);
        i.seek(0);
        h.check(i.readLong() == -6373734025067659118L);
        i.seek(0);
        h.check(i.readShort() == -32622);
        i.seek(0);
        h.check(i.readUnsignedByte() == 146);
        i.seek(0);
        h.check(i.readUnsignedInt() == 1996849298);
        i.seek(0);
        h.check(i.readUnsignedShort() == 32914);

        // Test flush().

        i.seek(4);

        h.check(i.getStreamPosition() == 4);

        i.flush();

        h.check(i.getFlushedPosition() == 4);
        
        boolean exceptionThrown = false;
        try
          {
            i.flushBefore(3);
          }
        catch (IndexOutOfBoundsException e)
          {
            exceptionThrown = true;
          }
        h.check(exceptionThrown);

        exceptionThrown = false;
        try
          {
            i.flushBefore(5);
          }
        catch (IndexOutOfBoundsException e)
          {
            exceptionThrown = true;
          }
        h.check(exceptionThrown);

        exceptionThrown = false;
        try
          {
            i.seek(2);
          }
        catch (IndexOutOfBoundsException e)
          {
            exceptionThrown = true;
          }
        h.check(exceptionThrown);

        exceptionThrown = false;
        try
          {
            i.seek(3);
          }
        catch (IndexOutOfBoundsException e)
          {
            exceptionThrown = true;
          }
        h.check(exceptionThrown);

        exceptionThrown = false;
        try
          {
            i.setBitOffset(-1);
          }
        catch (IllegalArgumentException e)
          {
            exceptionThrown = true;
          }
        h.check(exceptionThrown);

        exceptionThrown = false;
        try
          {
            i.setBitOffset(8);
          }
        catch (IllegalArgumentException e)
          {
            exceptionThrown = true;
          }
        h.check(exceptionThrown);

        i.setBitOffset(4);
        h.check(i.getBitOffset() == 4);

        // A MemoryCacheImageInputStream is cached in memory.
        h.check(i.isCached() == true);
        h.check(i.isCachedFile() == false);
        h.check(i.isCachedMemory() == true);

        h.check(i.length() == -1);

        // Test mark() and reset().
        i.seek(4);
        i.mark();

        i.read();
        i.read();

        i.reset();
        h.check(i.getStreamPosition() == 4);

        // Test readBit().
        // We're currently at b[4], byte 0xac.
        h.check(i.readBit() == 1);
        h.check(i.readBit() == 0);
        h.check(i.readBit() == 1);
        h.check(i.readBit() == 0);
        h.check(i.readBit() == 1);
        h.check(i.readBit() == 1);
        h.check(i.readBit() == 0);

        // Check that the bit offset is incremented.
        h.check(i.getBitOffset() == 7);

        // Roll back the bit offset within the same byte.
        i.setBitOffset(2);

        // R-read some bits within the same byte.
        h.check(i.readBit() == 1);
        h.check(i.readBit() == 0);
        h.check(i.readBit() == 1);
        h.check(i.readBit() == 1);
        h.check(i.readBit() == 0);

        // Check that the stream position is still 4.
        h.check(i.getStreamPosition() == 4);

        // Read the final bit in b[4].
        h.check(i.readBit() == 0);

        // Check that readBit on the 8th bit increments the stream
        // position.
        h.check(i.getStreamPosition() == 5);

        // Read the bits from b[5], byte 0xf2.
        h.check(i.readBit() == 1);
        h.check(i.readBit() == 1);
        h.check(i.readBit() == 1);
        h.check(i.readBit() == 1);

        h.check(i.getBitOffset() == 4);

        h.check(i.readBit() == 0);
        h.check(i.readBit() == 0);
        h.check(i.readBit() == 1);
        h.check(i.readBit() == 0);

        // Check that the bit offset is reset and the position
        // incremented.
        h.check(i.getBitOffset() == 0);
        h.check(i.getStreamPosition() == 6);

        h.check(i.length() == -1);

        // Test close().
        i.close();

        // Test checkClosed().
        exceptionThrown = false;
        try
          {
            i.close();
          }
        catch (IOException e)
          {
            exceptionThrown = true;
          }
        h.check(exceptionThrown);

        // 64 bytes of data.
        b = new byte[]
          { 
            (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
            (byte) 0xac, (byte) 0xf2, (byte) 0x8b, (byte) 0xa7,
            (byte) 0x5c, (byte) 0xd0, (byte) 0xaa, (byte) 0x0f,
            (byte) 0x89, (byte) 0x00, (byte) 0x12, (byte) 0xf1,
            (byte) 0xa1, (byte) 0xef, (byte) 0x82, (byte) 0x00,
            (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
            (byte) 0xac, (byte) 0xf2, (byte) 0x8b, (byte) 0xa7,
            (byte) 0x5c, (byte) 0xd0, (byte) 0xaa, (byte) 0x0f,
            (byte) 0x89, (byte) 0x00, (byte) 0x12, (byte) 0xf1,
            (byte) 0xa1, (byte) 0xef, (byte) 0x82, (byte) 0x00,
            (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
            (byte) 0xac, (byte) 0xf2, (byte) 0x8b, (byte) 0xa7,
            (byte) 0x5c, (byte) 0xd0, (byte) 0xaa, (byte) 0x0f,
            (byte) 0x89, (byte) 0x00, (byte) 0x12, (byte) 0xf1,
            (byte) 0xa1, (byte) 0xef, (byte) 0x82, (byte) 0x00,
            (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
          };

        bs = new ByteArrayInputStream(b);
        i = new MemoryCacheImageInputStream(bs);

        // Test readBits().
        i.seek(5);
        i.setBitOffset(6);

        exceptionThrown = false;
        try
          {
            i.readBits(-1);
          }
        catch (IllegalArgumentException e)
          {
            exceptionThrown = true;
          }
        h.check(exceptionThrown);

        exceptionThrown = false;
        try
          {
            i.readBits(65);
          }
        catch (IllegalArgumentException e)
          {
            exceptionThrown = true;
          }
        h.check(exceptionThrown);

        h.check(i.readBits(59) == 366848453836545810L);

        i.seek(5);
        i.setBitOffset(6);
        h.check(i.readBits(58) == 183424226918272905L);

        b = new byte[]
          {
            (byte) 0xa2, (byte) 0xe9, (byte) 0xd7, (byte) 0x34,
            (byte) 0x2a, (byte) 0x83, (byte) 0xe2, (byte) 0x40
          };
        bs = new ByteArrayInputStream(b);
        i = new MemoryCacheImageInputStream(bs);
        h.check(i.readBits(59) == 366848453836545810L);

        b = new byte[]
          {
            (byte) 0xa2, (byte) 0x02
          };
        bs = new ByteArrayInputStream(b);
        i = new MemoryCacheImageInputStream(bs);

        long[] res = new long[8];

        i.seek(0);

        h.check(i.readBits(0) == 0);
        i.seek(0);

        for (k = 0; k < 8; k++)
          {
            i.setBitOffset(k);
            res[k] = i.readBits(8);
            i.seek(0);
          }
        i.seek(0);
        h.check(Arrays.equals(res, new long[] { 162, 68, 136, 16,
                                                32, 64, 128, 1 }));

        // 64 bytes of data.
        b = new byte[]
          { 
            (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
            (byte) 0xac, (byte) 0xf2, (byte) 0x8b, (byte) 0xa7,
            (byte) 0x5c, (byte) 0xd0, (byte) 0xaa, (byte) 0x0f,
            // Here is a two-character-long UTF String to be read by
            // DataInputStream.
            (byte) 0x00, (byte) 0x02, (byte) 0x12, (byte) 0x21,
            (byte) 0xa1, (byte) 0xef, (byte) 0x82, (byte) 0x00,
            (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
            (byte) 0xac, (byte) 0xf2, (byte) 0x8b, (byte) 0xa7,
            (byte) 0x5c, (byte) 0xd0, (byte) 0xaa, (byte) 0x0f,
            (byte) 0x89, (byte) 0x00, (byte) 0x12, (byte) 0xf1,
            (byte) 0xa1, (byte) 0xef, (byte) 0x82, (byte) 0x00,
            (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
            (byte) 0xac, (byte) 0xf2, (byte) 0x8b, (byte) 0xa7,
            (byte) 0x5c, (byte) 0xd0, (byte) 0xaa, (byte) 0x0f,
            (byte) 0x89, (byte) 0x00, (byte) 0x12, (byte) 0xf1,
            (byte) 0xa1, (byte) 0xef, (byte) 0x82, (byte) 0x00,
            (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
          };

        bs = new ByteArrayInputStream(b);
        i = new MemoryCacheImageInputStream(bs);

        // Test mark() and reset().
        i.seek(17);
        h.check(i.getStreamPosition() == 17);
        i.mark();
        i.seek(49);
        h.check(i.getStreamPosition() == 49);
        i.reset();
        h.check(i.getStreamPosition() == 17);

        // Test skipBytes().
        i.setBitOffset(3);
        i.skipBytes(20);
        h.check(i.getStreamPosition() == 37);
        h.check(i.getBitOffset() == 0);

        // Test readUTF().
        i.seek(12);
        String str = i.readUTF();
        h.check(str.codePointAt(0) == 18);
        h.check(str.codePointAt(1) == 33);

        b = new byte[]
          { (byte) 0x47, (byte) 0x4e, (byte) 0x55,
            '\r',
            (byte) 0x43, (byte) 0x6c, (byte) 0x61, (byte) 0x73, (byte) 0x73,
            (byte) 0x70, (byte) 0x61, (byte) 0x74, (byte) 0x68,
            '\r', '\n',
            (byte) 0x52, (byte) 0x75, (byte) 0x6c,
            (byte) 0x65, (byte) 0x7a,
            '\n',
            (byte) 0x44, (byte) 0x75, (byte) 0x64, (byte) 0x65, (byte) 0x7a,
            (byte) 0x21,
            '\r'
          };

        bs = new ByteArrayInputStream(b);
        i = new MemoryCacheImageInputStream(bs);

        h.check(i.readLine().equals("GNU"));
        h.check(i.readLine().equals("Classpath"));
        h.check(i.readLine().equals("Rulez"));
        h.check(i.readLine().equals("Dudez!"));
        h.check(i.readLine() == null);

        // 64 bytes of data.
        b = new byte[]
          { 
            (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
            (byte) 0xac, (byte) 0xf2, (byte) 0x8b, (byte) 0xa7,
            (byte) 0x5c, (byte) 0xd0, (byte) 0xaa, (byte) 0x0f,
            (byte) 0x89, (byte) 0x00, (byte) 0x12, (byte) 0xf1,
            (byte) 0xa1, (byte) 0xef, (byte) 0x82, (byte) 0x00,
            (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
            (byte) 0xac, (byte) 0xf2, (byte) 0x8b, (byte) 0xa7,
            (byte) 0x5c, (byte) 0xd0, (byte) 0xaa, (byte) 0x0f,
            (byte) 0x89, (byte) 0x00, (byte) 0x12, (byte) 0xf1,
            (byte) 0xa1, (byte) 0xef, (byte) 0x82, (byte) 0x00,
            (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
            (byte) 0xac, (byte) 0xf2, (byte) 0x8b, (byte) 0xa7,
            (byte) 0x5c, (byte) 0xd0, (byte) 0xaa, (byte) 0x0f,
            (byte) 0x89, (byte) 0x00, (byte) 0x12, (byte) 0xf1,
            (byte) 0xa1, (byte) 0xef, (byte) 0x82, (byte) 0x00,
            (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
          };

        bs = new ByteArrayInputStream(b);
        i = new MemoryCacheImageInputStream(bs);

        byte[] fullB = new byte[26];
        i.seek(0);
        i.readFully(fullB);
        h.check(Arrays.equals(fullB,
                              new byte[]
            {
              (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
              (byte) 0xac, (byte) 0xf2, (byte) 0x8b, (byte) 0xa7,
              (byte) 0x5c, (byte) 0xd0, (byte) 0xaa, (byte) 0x0f,
              (byte) 0x89, (byte) 0x00, (byte) 0x12, (byte) 0xf1,
              (byte) 0xa1, (byte) 0xef, (byte) 0x82, (byte) 0x00,
              (byte) 0x92, (byte) 0x80, (byte) 0x05, (byte) 0x77,
              (byte) 0xac, (byte) 0xf2
            }));

        for (k = 0; k < fullB.length; k++)
          fullB[k] = 0;

        i.seek(0);
        i.readFully(fullB, 5, 13);

        h.check(Arrays.equals(fullB, new byte[]
            {
              (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
              (byte) 0x00, (byte) 0x92, (byte) 0x80, (byte) 0x05,
              (byte) 0x77, (byte) 0xac, (byte) 0xf2, (byte) 0x8b,
              (byte) 0xa7, (byte) 0x5c, (byte) 0xd0, (byte) 0xaa,
              (byte) 0x0f, (byte) 0x89, (byte) 0x00, (byte) 0x00,
              (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
              (byte) 0x00, (byte) 0x00
            }));

        char[] fullC = new char[15];
        i.seek(0);
        i.readFully(fullC, 0, 15);

        h.check(Arrays.equals(fullC, new char[]
            {
              (char) 37504, (char) 1399, (char) 44274,
              (char) 35751, (char) 23760, (char) 43535,
              (char) 35072, (char) 4849, (char) 41455,
              (char) 33280, (char) 37504, (char) 1399,
              (char) 44274, (char) 35751, (char) 23760
            }));

        double[] fullD = new double[8];
        i.seek(0);
        i.readFully(fullD, 0, 8);

        h.check(Arrays.equals(fullD, new double[] {
                                -1.4183142849706364E-219,
                                1.2402952421911034E139,
                                -3.154063812740471E-145,
                                -3.556316535750171E-92,
                                -2.4925149951304603E-265,
                                -1.4183142849706364E-219,
                                1.2402952421911034E139,
                                -3.154063812740471E-145
                              }));

        float[] fullF = new float[16];
        i.seek(0);
        i.readFully(fullF, 0, 16);

        h.check(Arrays.equals(fullF, new float[] {
                                -8.079283E-28f,
                                -6.893558E-12f,
                                4.69870212E17f,
                                -1.5416346E-33f,
                                -1.6229681E-18f,
                                -8.079283E-28f,
                                -6.893558E-12f,
                                4.69870212E17f,
                                -1.5416346E-33f,
                                -1.6229681E-18f,
                                -8.079283E-28f,
                                -6.893558E-12f,
                                4.69870212E17f,
                                -1.5416346E-33f,
                                -1.6229681E-18f,
                                -8.079283E-28f
                              }));

        int[] fullI = new int[16];
        i.seek(0);
        i.readFully(fullI, 0, 16);

        h.check(Arrays.equals(fullI, new int[] {
                                -1837103753,
                                -1393390681,
                                1557178895,
                                -1996483855,
                                -1578139136,
                                -1837103753,
                                -1393390681,
                                1557178895,
                                -1996483855,
                                -1578139136,
                                -1837103753,
                                -1393390681,
                                1557178895,
                                -1996483855,
                                -1578139136,
                                -1837103753,
                              }));

        long[] fullL = new long[8];
        i.seek(0);
        i.readFully(fullL, 0, 8);

        h.check(Arrays.equals(fullL, new long[] {
                                -7890300535592285273L,
                                6688032430344901361L,
                                -6778055975199832713L,
                                -5984567403888989681L,
                                -8574832861500177920L,
                                -7890300535592285273L,
                                6688032430344901361L,
                                -6778055975199832713L,
                              }));

        short[] fullS = new short[32];
        i.seek(0);
        i.readFully(fullS, 0, 32);

        h.check(Arrays.equals(fullS, new short[] {
                                -28032,
                                1399,
                                -21262,
                                -29785,
                                23760,
                                -22001,
                                -30464,
                                4849,
                                -24081,
                                -32256,
                                -28032,
                                1399,
                                -21262,
                                -29785,
                                23760,
                                -22001,
                                -30464,
                                4849,
                                -24081,
                                -32256,
                                -28032,
                                1399,
                                -21262,
                                -29785,
                                23760,
                                -22001,
                                -30464,
                                4849,
                                -24081,
                                -32256,
                                -28032,
                                1399,
                              }));
      }
    catch(IOException e)
      {
        throw new RuntimeException(e);
      }
  }
}
