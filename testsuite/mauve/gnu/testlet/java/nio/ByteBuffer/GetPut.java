// Tags: JDK1.4

// Copyright (C) 2004 Max Gilead <gilead@yellowhedgehog.com>

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

package gnu.testlet.java.nio.ByteBuffer;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class GetPut implements Testlet
{
  public void test(TestHarness h)
  {
    relativePut(h);
    relativeGet(h);
    bulkPut(h);
    bulkGet(h);
    bufferPut(h);
    absolutePut(h);
    absoluteGet(h);
    specialValues(h);
  }

  private void relativePut(TestHarness h)
  {
    ByteBuffer buf = null;

    byte[] arr = new byte[200];
    buf = ByteBuffer.wrap(arr);
    buf.order(ByteOrder.BIG_ENDIAN).   put((byte)1);
    buf.order(ByteOrder.LITTLE_ENDIAN).put((byte)2);
    buf.order(ByteOrder.BIG_ENDIAN).   putShort((short)3);
    buf.order(ByteOrder.LITTLE_ENDIAN).putShort((short)4);
    buf.order(ByteOrder.BIG_ENDIAN).   putInt(5);
    buf.order(ByteOrder.LITTLE_ENDIAN).putInt(6);
    buf.order(ByteOrder.BIG_ENDIAN).   putLong(7);
    buf.order(ByteOrder.LITTLE_ENDIAN).putLong(8);
    buf.order(ByteOrder.BIG_ENDIAN).   putFloat(9.0f);
    buf.order(ByteOrder.LITTLE_ENDIAN).putFloat(10.0f);
    buf.order(ByteOrder.BIG_ENDIAN).   putDouble(11.0);
    buf.order(ByteOrder.LITTLE_ENDIAN).putDouble(12.0);
    buf.order(ByteOrder.BIG_ENDIAN).   putChar('a');
    buf.order(ByteOrder.LITTLE_ENDIAN).putChar('b');
    buf.order(ByteOrder.BIG_ENDIAN).   put((byte)0xf1);
    buf.order(ByteOrder.LITTLE_ENDIAN).put((byte)0xf2);
    buf.order(ByteOrder.BIG_ENDIAN).   putShort((short)0xfff3);
    buf.order(ByteOrder.LITTLE_ENDIAN).putShort((short)0xfff4);
    buf.order(ByteOrder.BIG_ENDIAN).   putInt(0xfffffff5);
    buf.order(ByteOrder.LITTLE_ENDIAN).putInt(0xfffffff6);
    buf.order(ByteOrder.BIG_ENDIAN).   putLong(0xfffffffffffffff7L);
    buf.order(ByteOrder.LITTLE_ENDIAN).putLong(0xfffffffffffffff8L);
    buf.order(ByteOrder.BIG_ENDIAN).   putFloat(Float.NEGATIVE_INFINITY);
    buf.order(ByteOrder.LITTLE_ENDIAN).putFloat(Float.NEGATIVE_INFINITY);
    buf.order(ByteOrder.BIG_ENDIAN).   putDouble(Double.NEGATIVE_INFINITY);
    buf.order(ByteOrder.LITTLE_ENDIAN).putDouble(Double.NEGATIVE_INFINITY);
    buf.order(ByteOrder.BIG_ENDIAN).   putChar('\ufffd');
    buf.order(ByteOrder.LITTLE_ENDIAN).putChar('\ufffe');

    h.checkPoint("relative put");
    checkArray(h, TEST_ARRAY, arr);
  }
  private void relativeGet(TestHarness h)
  {
    ByteBuffer buf = null;

    byte[] arr = new byte[200];
    for (int i = 0; i < TEST_ARRAY.length; i++)
    {
      arr[i] = TEST_ARRAY[i];
    }
    buf = ByteBuffer.wrap(arr);
    h.checkPoint("relative get");
    h.check(buf.order(ByteOrder.BIG_ENDIAN).   get(),      1);
    h.check(buf.order(ByteOrder.LITTLE_ENDIAN).get(),      2);
    h.check(buf.order(ByteOrder.BIG_ENDIAN).   getShort(), 3);
    h.check(buf.order(ByteOrder.LITTLE_ENDIAN).getShort(), 4);
    h.check(buf.order(ByteOrder.BIG_ENDIAN).   getInt(),   5);
    h.check(buf.order(ByteOrder.LITTLE_ENDIAN).getInt(),   6);
    h.check(buf.order(ByteOrder.BIG_ENDIAN).   getLong(),  7);
    h.check(buf.order(ByteOrder.LITTLE_ENDIAN).getLong(),  8);
    h.check(Float.floatToIntBits(buf.order(ByteOrder.BIG_ENDIAN).getFloat()),
      Float.floatToIntBits(9.0f));
    h.check(Float.floatToIntBits(buf.order(ByteOrder.LITTLE_ENDIAN).getFloat()),
      Float.floatToIntBits(10.0f));
    h.check(Double.doubleToLongBits(buf.order(ByteOrder.BIG_ENDIAN).getDouble()),
      Double.doubleToLongBits(11.0));
    h.check(Double.doubleToLongBits(buf.order(ByteOrder.LITTLE_ENDIAN).getDouble()),
      Double.doubleToLongBits(12.0));
    h.check((int)buf.order(ByteOrder.BIG_ENDIAN).   getChar(), (int)'a');
    h.check((int)buf.order(ByteOrder.LITTLE_ENDIAN).getChar(), (int)'b');
    h.check(buf.order(ByteOrder.BIG_ENDIAN).   get(),      (byte)0xf1);
    h.check(buf.order(ByteOrder.LITTLE_ENDIAN).get(),      (byte)0xf2);
    h.check(buf.order(ByteOrder.BIG_ENDIAN).   getShort(), (short)0xfff3);
    h.check(buf.order(ByteOrder.LITTLE_ENDIAN).getShort(), (short)0xfff4);
    h.check(buf.order(ByteOrder.BIG_ENDIAN).   getInt(),   0xfffffff5);
    h.check(buf.order(ByteOrder.LITTLE_ENDIAN).getInt(),   0xfffffff6);
    h.check(buf.order(ByteOrder.BIG_ENDIAN).   getLong(),  0xfffffffffffffff7L);
    h.check(buf.order(ByteOrder.LITTLE_ENDIAN).getLong(),  0xfffffffffffffff8L);
    h.check(Float.floatToIntBits(buf.order(ByteOrder.BIG_ENDIAN).getFloat()),
      Float.floatToIntBits(Float.NEGATIVE_INFINITY));
    h.check(Float.floatToIntBits(buf.order(ByteOrder.LITTLE_ENDIAN).getFloat()),
      Float.floatToIntBits(Float.NEGATIVE_INFINITY));
    h.check(Double.doubleToLongBits(buf.order(ByteOrder.BIG_ENDIAN).getDouble()),
      Double.doubleToLongBits(Double.NEGATIVE_INFINITY));
    h.check(Double.doubleToLongBits(buf.order(ByteOrder.LITTLE_ENDIAN).getDouble()),
      Double.doubleToLongBits(Double.NEGATIVE_INFINITY));
    h.check((int)buf.order(ByteOrder.BIG_ENDIAN).   getChar(), (int)'\ufffd');
    h.check((int)buf.order(ByteOrder.LITTLE_ENDIAN).getChar(), (int)'\ufffe');
  }
  private void bulkPut(TestHarness h)
  {
    ByteBuffer buf = null;

    byte[] arr = new byte[6];
    buf = ByteBuffer.wrap(arr);

    buf.order(ByteOrder.BIG_ENDIAN);
    buf.put(new byte[] { 1, 2, 0, 0, 5, 6 });
    buf.position(2);
    buf.order(ByteOrder.LITTLE_ENDIAN);
    buf.put(new byte[] { 0, 0, 3, 4, 0, 0 }, 2, 2);

    h.checkPoint("bulk put");
    checkArray(h, arr, new byte[] { 1, 2, 3, 4, 5, 6 });
  }
  private void bulkGet(TestHarness h)
  {
    ByteBuffer buf = null;

    byte[] arr = new byte[] { 1, 2, 0, 0, 5, 6, 3, 4 };
    buf = ByteBuffer.wrap(arr);

    byte[] readArr = new byte[6];
    buf.order(ByteOrder.BIG_ENDIAN);
    buf.get(readArr);
    buf.position(6);
    buf.order(ByteOrder.LITTLE_ENDIAN);
    buf.get(readArr, 2, 2);

    h.checkPoint("bulk get");
    checkArray(h, readArr, new byte[] { 1, 2, 3, 4, 5, 6 });
  }
  private void bufferPut(TestHarness h)
  {
    ByteBuffer buf = null;

    byte[] arr = new byte[6];
    buf = ByteBuffer.wrap(arr);

    buf.order(ByteOrder.BIG_ENDIAN);
    buf.put(ByteBuffer.wrap(new byte[] { 1, 2, 0, 0, 5, 6 }));
    buf.position(2);
    buf.order(ByteOrder.LITTLE_ENDIAN);
    buf.put(ByteBuffer.wrap(new byte[] { 3, 4 }));

    h.checkPoint("buffer put");
    checkArray(h, arr, new byte[] { 1, 2, 3, 4, 5, 6 });
  }
  private void absolutePut(TestHarness h)
  {
    ByteBuffer buf = null;

    byte[] arr = new byte[200];
    buf = ByteBuffer.wrap(arr);
    buf.order(ByteOrder.BIG_ENDIAN);
    buf.putChar(   54, 'a');
    buf.putDouble( 38, 11.0);
    buf.putFloat(  30, 9.0f);
    buf.putLong(   14, 7);
    buf.putInt(     6, 5);
    buf.putShort(   2, (short)3);
    buf.put(        0, (byte)1);
    buf.putChar(  112, '\ufffd');
    buf.putDouble( 96, Double.NEGATIVE_INFINITY);
    buf.putFloat(  88, Float.NEGATIVE_INFINITY);
    buf.putLong(   72, 0xfffffffffffffff7L);
    buf.putInt(    64, 0xfffffff5);
    buf.putShort(  60, (short)0xfff3);
    buf.put(       58, (byte)0xf1);
    buf.order(ByteOrder.LITTLE_ENDIAN);
    buf.put(        1, (byte)2);
    buf.putShort(   4, (short)4);
    buf.putInt(    10, 6);
    buf.putLong(   22, 8);
    buf.putFloat(  34, 10.0f);
    buf.putDouble( 46, 12.0);
    buf.putChar(   56, 'b');
    buf.putLong(   80, 0xfffffffffffffff8L);
    buf.putInt(    68, 0xfffffff6);
    buf.putShort(  62, (short)0xfff4);
    buf.put(       59, (byte)0xf2);
    buf.putFloat(  92, Float.NEGATIVE_INFINITY);
    buf.putDouble(104, Double.NEGATIVE_INFINITY);
    buf.putChar(  114, '\ufffe');

    h.checkPoint("absolute put");
    checkArray(h, TEST_ARRAY, arr);
  }
  private void absoluteGet(TestHarness h)
  {
    ByteBuffer buf = null;

    byte[] arr = new byte[200];
    for (int i = 0; i < TEST_ARRAY.length; i++)
    {
      arr[i] = TEST_ARRAY[i];
    }

    buf = ByteBuffer.wrap(arr);
    h.checkPoint("absolute get");
    buf.order(ByteOrder.BIG_ENDIAN);
    h.check((int)buf.getChar(54), (int)'a');
    h.check(Double.doubleToLongBits(buf.getDouble(38)), Double.doubleToLongBits(11.0));
    h.check(Float.floatToIntBits(buf.getFloat(30)), Float.floatToIntBits(9.0f));
    h.check(buf.getLong(14), 7);
    h.check(buf.getInt(6),   5);
    h.check(buf.getShort(2), 3);
    h.check(buf.get(0),      1);
    h.check((int)buf.getChar(112), (int)'\ufffd');
    h.check(Double.doubleToLongBits(buf.getDouble(96)), Double.doubleToLongBits(Double.NEGATIVE_INFINITY));
    h.check(Float.floatToIntBits(buf.getFloat(88)), Float.floatToIntBits(Float.NEGATIVE_INFINITY));
    h.check(buf.getLong(72), 0xfffffffffffffff7L);
    h.check(buf.getInt(64),  0xfffffff5);
    h.check(buf.getShort(60),(short)0xfff3);
    h.check(buf.get(58),     (byte)0xf1);

    buf.order(ByteOrder.LITTLE_ENDIAN);
    h.check(buf.get(1),      2);
    h.check(buf.getShort(4), 4);
    h.check(buf.getInt(10),  6);
    h.check(buf.getLong(22), 8);
    h.check(Float.floatToIntBits(buf.getFloat(34)), Float.floatToIntBits(10.0f));
    h.check(Double.doubleToLongBits(buf.getDouble(46)), Double.doubleToLongBits(12.0));
    h.check((int)buf.getChar(56), (int)'b');
    h.check(buf.get(59),     (byte)0xf2);
    h.check(buf.getShort(62),(byte)0xfff4);
    h.check(buf.getInt(68),  0xfffffff6);
    h.check(buf.getLong(80), 0xfffffffffffffff8L);
    h.check(Float.floatToIntBits(buf.getFloat(92)), Float.floatToIntBits(Float.NEGATIVE_INFINITY));
    h.check(Double.doubleToLongBits(buf.getDouble(104)), Double.doubleToLongBits(Double.NEGATIVE_INFINITY));
    h.check((int)buf.getChar(114), (int)'\ufffe');
  }
  private void specialValues(TestHarness h)
  {
    ByteBuffer buf = null;

    byte[] arr = new byte[200];
    buf = ByteBuffer.wrap(arr);

    h.checkPoint("special values");
    buf.order(ByteOrder.BIG_ENDIAN);
    buf.put(Byte.MIN_VALUE);
    buf.put(Byte.MAX_VALUE);
    buf.putShort(Short.MIN_VALUE);
    buf.putShort(Short.MAX_VALUE);
    buf.putInt(Integer.MIN_VALUE);
    buf.putInt(Integer.MAX_VALUE);
    buf.putLong(Long.MIN_VALUE);
    buf.putLong(Long.MAX_VALUE);
    buf.putFloat(Float.MIN_VALUE);
    buf.putFloat(Float.MAX_VALUE);
    buf.putFloat(Float.NaN);
    buf.putFloat(Float.NEGATIVE_INFINITY);
    buf.putFloat(Float.POSITIVE_INFINITY);
    buf.putDouble(Double.MIN_VALUE);
    buf.putDouble(Double.MAX_VALUE);
    buf.putDouble(Double.NaN);
    buf.putDouble(Double.NEGATIVE_INFINITY);
    buf.putDouble(Double.POSITIVE_INFINITY);
    buf.order(ByteOrder.LITTLE_ENDIAN);
    buf.put(Byte.MIN_VALUE);
    buf.put(Byte.MAX_VALUE);
    buf.putShort(Short.MIN_VALUE);
    buf.putShort(Short.MAX_VALUE);
    buf.putInt(Integer.MIN_VALUE);
    buf.putInt(Integer.MAX_VALUE);
    buf.putLong(Long.MIN_VALUE);
    buf.putLong(Long.MAX_VALUE);
    buf.putFloat(Float.MIN_VALUE);
    buf.putFloat(Float.MAX_VALUE);
    buf.putFloat(Float.NaN);
    buf.putFloat(Float.NEGATIVE_INFINITY);
    buf.putFloat(Float.POSITIVE_INFINITY);
    buf.putDouble(Double.MIN_VALUE);
    buf.putDouble(Double.MAX_VALUE);
    buf.putDouble(Double.NaN);
    buf.putDouble(Double.NEGATIVE_INFINITY);
    buf.putDouble(Double.POSITIVE_INFINITY);
    buf.rewind();
    buf.order(ByteOrder.BIG_ENDIAN);
    h.check(buf.get(), Byte.MIN_VALUE);
    h.check(buf.get(), Byte.MAX_VALUE);
    h.check(buf.getShort(), Short.MIN_VALUE);
    h.check(buf.getShort(), Short.MAX_VALUE);
    h.check(buf.getInt(), Integer.MIN_VALUE);
    h.check(buf.getInt(), Integer.MAX_VALUE);
    h.check(buf.getLong(), Long.MIN_VALUE);
    h.check(buf.getLong(), Long.MAX_VALUE);
    h.check(buf.getFloat(), Float.MIN_VALUE);
    h.check(buf.getFloat(), Float.MAX_VALUE);
    h.check(buf.getFloat(), Float.NaN);
    h.check(buf.getFloat(), Float.NEGATIVE_INFINITY);
    h.check(buf.getFloat(), Float.POSITIVE_INFINITY);
    h.check(buf.getDouble(), Double.MIN_VALUE);
    h.check(buf.getDouble(), Double.MAX_VALUE);
    h.check(buf.getDouble(), Double.NaN);
    h.check(buf.getDouble(), Double.NEGATIVE_INFINITY);
    h.check(buf.getDouble(), Double.POSITIVE_INFINITY);
    buf.order(ByteOrder.LITTLE_ENDIAN);
    h.check(buf.get(), Byte.MIN_VALUE);
    h.check(buf.get(), Byte.MAX_VALUE);
    h.check(buf.getShort(), Short.MIN_VALUE);
    h.check(buf.getShort(), Short.MAX_VALUE);
    h.check(buf.getInt(), Integer.MIN_VALUE);
    h.check(buf.getInt(), Integer.MAX_VALUE);
    h.check(buf.getLong(), Long.MIN_VALUE);
    h.check(buf.getLong(), Long.MAX_VALUE);
    h.check(buf.getFloat(), Float.MIN_VALUE);
    h.check(buf.getFloat(), Float.MAX_VALUE);
    h.check(buf.getFloat(), Float.NaN);
    h.check(buf.getFloat(), Float.NEGATIVE_INFINITY);
    h.check(buf.getFloat(), Float.POSITIVE_INFINITY);
    h.check(buf.getDouble(), Double.MIN_VALUE);
    h.check(buf.getDouble(), Double.MAX_VALUE);
    h.check(buf.getDouble(), Double.NaN);
    h.check(buf.getDouble(), Double.NEGATIVE_INFINITY);
    h.check(buf.getDouble(), Double.POSITIVE_INFINITY);
  }


  private static final byte[] TEST_ARRAY = new byte[116];
  static
  {
    TEST_ARRAY[ 0] = 1; // (byte)1
    TEST_ARRAY[ 1] = 2; // (byte)2
    TEST_ARRAY[ 2] = 0; // (short)3
    TEST_ARRAY[ 3] = 3;
    TEST_ARRAY[ 4] = 4; // (short)4
    TEST_ARRAY[ 5] = 0;
    TEST_ARRAY[ 6] = 0; // (int)5
    TEST_ARRAY[ 7] = 0;
    TEST_ARRAY[ 8] = 0;
    TEST_ARRAY[ 9] = 5;
    TEST_ARRAY[10] = 6; // (int)6
    TEST_ARRAY[11] = 0;
    TEST_ARRAY[12] = 0;
    TEST_ARRAY[13] = 0;
    TEST_ARRAY[14] = 0; // (long)7
    TEST_ARRAY[15] = 0;
    TEST_ARRAY[16] = 0;
    TEST_ARRAY[17] = 0;
    TEST_ARRAY[18] = 0;
    TEST_ARRAY[19] = 0;
    TEST_ARRAY[20] = 0;
    TEST_ARRAY[21] = 7;
    TEST_ARRAY[22] = 8; // (long)8
    TEST_ARRAY[23] = 0;
    TEST_ARRAY[24] = 0;
    TEST_ARRAY[25] = 0;
    TEST_ARRAY[26] = 0;
    TEST_ARRAY[27] = 0;
    TEST_ARRAY[28] = 0;
    TEST_ARRAY[29] = 0;
    int f9 = Float.floatToIntBits(9.0f);
    TEST_ARRAY[30] = (byte)(f9 >> 24 & 0xff); // (float)9
    TEST_ARRAY[31] = (byte)(f9 >> 16 & 0xff);
    TEST_ARRAY[32] = (byte)(f9 >>  8 & 0xff);
    TEST_ARRAY[33] = (byte)(f9       & 0xff);
    int f10 = Float.floatToIntBits(10.0f);
    TEST_ARRAY[34] = (byte)(f10       & 0xff); // (float)10
    TEST_ARRAY[35] = (byte)(f10 >>  8 & 0xff);
    TEST_ARRAY[36] = (byte)(f10 >> 16 & 0xff);
    TEST_ARRAY[37] = (byte)(f10 >> 24 & 0xff);
    long d11 = Double.doubleToLongBits(11.0);
    TEST_ARRAY[38] = (byte)(d11 >> 56 & 0xff); // (double)11
    TEST_ARRAY[39] = (byte)(d11 >> 48 & 0xff);
    TEST_ARRAY[40] = (byte)(d11 >> 40 & 0xff);
    TEST_ARRAY[41] = (byte)(d11 >> 32 & 0xff);
    TEST_ARRAY[42] = (byte)(d11 >> 24 & 0xff);
    TEST_ARRAY[43] = (byte)(d11 >> 16 & 0xff);
    TEST_ARRAY[44] = (byte)(d11 >>  8 & 0xff);
    TEST_ARRAY[45] = (byte)(d11       & 0xff);
    long d12 = Double.doubleToLongBits(12.0);
    TEST_ARRAY[46] = (byte)(d12       & 0xff); // (double)12
    TEST_ARRAY[47] = (byte)(d12 >>  8 & 0xff);
    TEST_ARRAY[48] = (byte)(d12 >> 16 & 0xff);
    TEST_ARRAY[49] = (byte)(d12 >> 24 & 0xff);
    TEST_ARRAY[50] = (byte)(d12 >> 32 & 0xff);
    TEST_ARRAY[51] = (byte)(d12 >> 40 & 0xff);
    TEST_ARRAY[52] = (byte)(d12 >> 48 & 0xff);
    TEST_ARRAY[53] = (byte)(d12 >> 56 & 0xff);
    TEST_ARRAY[54] = 0;         // 'a'
    TEST_ARRAY[55] = (byte)'a';
    TEST_ARRAY[56] = (byte)'b'; // 'b'
    TEST_ARRAY[57] = 0;
    TEST_ARRAY[58] = (byte)0xf1; // (byte)f1
    TEST_ARRAY[59] = (byte)0xf2; // (byte)f2
    TEST_ARRAY[60] = (byte)0xff; // (short)fff3
    TEST_ARRAY[61] = (byte)0xf3;
    TEST_ARRAY[62] = (byte)0xf4; // (short)fff4
    TEST_ARRAY[63] = (byte)0xff;
    TEST_ARRAY[64] = (byte)0xff; // (int)fffffff5
    TEST_ARRAY[65] = (byte)0xff;
    TEST_ARRAY[66] = (byte)0xff;
    TEST_ARRAY[67] = (byte)0xf5;
    TEST_ARRAY[68] = (byte)0xf6; // (int)fffffff6
    TEST_ARRAY[69] = (byte)0xff;
    TEST_ARRAY[70] = (byte)0xff;
    TEST_ARRAY[71] = (byte)0xff;
    TEST_ARRAY[72] = (byte)0xff; // (long)fffffffffffffff7
    TEST_ARRAY[73] = (byte)0xff;
    TEST_ARRAY[74] = (byte)0xff;
    TEST_ARRAY[75] = (byte)0xff;
    TEST_ARRAY[76] = (byte)0xff;
    TEST_ARRAY[77] = (byte)0xff;
    TEST_ARRAY[78] = (byte)0xff;
    TEST_ARRAY[79] = (byte)0xf7;
    TEST_ARRAY[80] = (byte)0xf8; // (long)fffffffffffffff8
    TEST_ARRAY[81] = (byte)0xff;
    TEST_ARRAY[82] = (byte)0xff;
    TEST_ARRAY[83] = (byte)0xff;
    TEST_ARRAY[84] = (byte)0xff;
    TEST_ARRAY[85] = (byte)0xff;
    TEST_ARRAY[86] = (byte)0xff;
    TEST_ARRAY[87] = (byte)0xff;
    int fNI = Float.floatToIntBits(Float.NEGATIVE_INFINITY);
    TEST_ARRAY[88] = (byte)(fNI >> 24 & 0xff); // (float)bits: NEGATIVE_INFINITY 0xff800000
    TEST_ARRAY[89] = (byte)(fNI >> 16 & 0x80);
    TEST_ARRAY[90] = (byte)(fNI >>  8 & 0x00);
    TEST_ARRAY[91] = (byte)(fNI       & 0x00);
    TEST_ARRAY[92] = (byte)(fNI       & 0x00); // (float)bits: NEGATIVE_INFINITY 0xff800000
    TEST_ARRAY[93] = (byte)(fNI >>  8 & 0x00);
    TEST_ARRAY[94] = (byte)(fNI >> 16 & 0x80);
    TEST_ARRAY[95] = (byte)(fNI >> 24 & 0xff);
    long dNI = Double.doubleToLongBits(Double.NEGATIVE_INFINITY);
    TEST_ARRAY[96] = (byte)(dNI >> 56 & 0xff); // (double)bits: NEGATIVE_INFINITY 0xfff0000000000000L
    TEST_ARRAY[97] = (byte)(dNI >> 48 & 0xf0);
    TEST_ARRAY[98] = (byte)(dNI >> 40 & 0x00);
    TEST_ARRAY[99] = (byte)(dNI >> 32 & 0x00);
    TEST_ARRAY[100] = (byte)(dNI >> 24 & 0x00);
    TEST_ARRAY[101] = (byte)(dNI >> 16 & 0x00);
    TEST_ARRAY[102] = (byte)(dNI >>  8 & 0x00);
    TEST_ARRAY[103] = (byte)(dNI       & 0x00);
    TEST_ARRAY[104] = (byte)(dNI       & 0x00); // (double)bits: NEGATIVE_INFINITY 0xfff0000000000000L
    TEST_ARRAY[105] = (byte)(dNI >>  8 & 0x00);
    TEST_ARRAY[106] = (byte)(dNI >> 16 & 0x00);
    TEST_ARRAY[107] = (byte)(dNI >> 24 & 0x00);
    TEST_ARRAY[108] = (byte)(dNI >> 32 & 0x00);
    TEST_ARRAY[109] = (byte)(dNI >> 40 & 0x00);
    TEST_ARRAY[110] = (byte)(dNI >> 48 & 0xf0);
    TEST_ARRAY[111] = (byte)(dNI >> 56 & 0xff);
    TEST_ARRAY[112] = (byte)0xff; // \ufffd
    TEST_ARRAY[113] = (byte)0xfd;
    TEST_ARRAY[114] = (byte)0xfe; // \ufffe
    TEST_ARRAY[115] = (byte)0xff;
  }
  private void checkArray(TestHarness h, byte[] arr0, byte[] arr1)
  {
    for (int i = 0; i < arr0.length; i++)
    {
      h.check(arr0[i], arr1[i]);
    }
  }
}
