// Tags: JDK1.2

// Copyright (C) 2005 Audrius Meskauskas (AudriusA@Bioinformatics.org)

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

package gnu.testlet.org.omg.CORBA.portable.InputStream;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.Asserter;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;

import java.math.BigDecimal;

import java.util.Random;

/**
 * This test checks the correctness of the CDR (comon data representation)
 * streams.
 *
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public class cdrIO
  extends Asserter
  implements Testlet
{
  ORB orb = ORB.init();
  Random r = new Random();
  private org.omg.CORBA.portable.OutputStream out;

  public void test(TestHarness harness)
  {
    h = harness;
    testRead_any();
    testRead_boolean();
    testRead_boolean_array();
    testRead_char();
    testRead_char_array();
    testRead_double();
    testRead_double_array();
    testRead_fixed();
    testRead_float();
    testRead_float_array();
    testRead_long();
    testRead_longlong();
    testRead_longlong_array();
    testRead_long_array();
    testRead_octet();
    testRead_octet_array();
    testRead_short();
    testRead_short_array();
    testRead_string();
    testRead_TypeCode();
    testRead_ulong();
    testRead_ulonglong();
    testRead_ulonglong_array();
    testRead_ulong_array();
    testRead_ushort();
    testRead_ushort_array();
    testRead_wchar();
    testRead_wchar_array();
    testRead_wstring();
  }

  public void testRead_TypeCode()
  {
    TypeCode expectedReturn = orb.create_fixed_tc((short) 12, (short) 5);

    out = out();
    out.write_TypeCode(expectedReturn);

    TypeCode actualReturn = out.create_input_stream().read_TypeCode();
    assertTrue("typecode", expectedReturn.equal(actualReturn));
  }

  public void testRead_any()
  {
    Any expectedReturn = orb.create_any();
    expectedReturn.insert_long(r.nextInt());

    out = out();
    out.write_any(expectedReturn);

    Any actualReturn = out.create_input_stream().read_any();

    assertEquals("Any enclosed value", expectedReturn.extract_long(),
                 actualReturn.extract_long()
                );

    assertTrue("Any, types", expectedReturn.type().equal(actualReturn.type()));

    expectedReturn = orb.create_any();
    expectedReturn.insert_string("http://www.akl.lt/remejai");

    out = out();
    out.write_any(expectedReturn);

    actualReturn = out.create_input_stream().read_any();

    assertEquals("Any enclosed value", expectedReturn.extract_string(),
                 actualReturn.extract_string()
                );
    assertTrue("Any, types", expectedReturn.type().equal(actualReturn.type()));
  }

  public void testRead_boolean()
  {
    out = out();
    out.write_boolean(true);
    assertTrue("boolean, true", out.create_input_stream().read_boolean());

    out = out();
    out.write_boolean(false);
    assertTrue("boolean, false", !(out.create_input_stream().read_boolean()));
  }

  public void testRead_boolean_array()
  {
    boolean[] x = new boolean[ 24 ];
    for (int i = 0; i < x.length; i++)
      {
        x [ i ] = r.nextBoolean();
      }

    int offs = r.nextInt(5);
    int len = r.nextInt(10) + 1;

    out = out();
    out.write_boolean_array(x, offs, len);

    boolean[] r = new boolean[ x.length ];

    out.create_input_stream().read_boolean_array(r, offs, len);

    boolean eq = true;
    for (int i = offs; i < offs + len; i++)
      {
        if (x [ i ] != r [ i ])
          {
            System.out.println(x [ i ] + "!=" + r [ i ] + " at " + i);
            eq = false;
          }
      }
    assertTrue("boolean array", eq);
  }

  public void testRead_char()
  {
    out = out();
    out.write_char('x');
    assertEquals("narrow char", out.create_input_stream().read_char(), 'x');
  }

  public void testRead_char_array()
  {
    char[] x = new char[ 24 ];
    for (int i = 0; i < x.length; i++)
      {
        x [ i ] = r.nextBoolean() ? 'a' : 'b';
      }

    int offs = r.nextInt(5);
    int len = r.nextInt(10) + 1;

    out = out();
    out.write_char_array(x, offs, len);

    char[] r = new char[ x.length ];

    out.create_input_stream().read_char_array(r, offs, len);

    boolean eq = true;
    for (int i = offs; i < offs + len; i++)
      {
        if (x [ i ] != r [ i ])
          eq = false;
      }
    assertTrue("char array", eq);
  }

  public void testRead_double()
  {
    double expectedReturn = r.nextDouble();
    out = out();
    out.write_double(expectedReturn);

    assertEquals("double", out.create_input_stream().read_double(),
                 expectedReturn, Double.MIN_VALUE
                );
  }

  public void testRead_double_array()
  {
    double[] x = new double[ 24 ];
    for (int i = 0; i < x.length; i++)
      {
        x [ i ] = r.nextDouble();
      }

    int offs = r.nextInt(5);
    int len = r.nextInt(10) + 1;

    out = out();
    out.write_double_array(x, offs, len);

    double[] r = new double[ x.length ];

    out.create_input_stream().read_double_array(r, offs, len);

    boolean eq = true;
    for (int i = offs; i < offs + len; i++)
      {
        if (x [ i ] != r [ i ])
          eq = false;
      }
    assertTrue("double array", eq);
  }

  public void testRead_fixed()
  {
    BigDecimal expectedReturn = new BigDecimal(r.nextInt());

    out = out();
    out.write_fixed(expectedReturn);

    BigDecimal actualReturn = out.create_input_stream().read_fixed();
    assertEquals("fixed " + expectedReturn + "!=" + actualReturn.toString(),
                 expectedReturn.toString(), actualReturn.toString()
                );
  }

  public void testRead_float()
  {
    float expectedReturn = r.nextFloat();
    out = out();
    out.write_float(expectedReturn);

    assertEquals("float", out.create_input_stream().read_float(),
                 expectedReturn, Float.MIN_VALUE
                );
  }

  public void testRead_float_array()
  {
    float[] x = new float[ 24 ];
    for (int i = 0; i < x.length; i++)
      {
        x [ i ] = r.nextFloat();
      }

    int offs = r.nextInt(5);
    int len = r.nextInt(10) + 1;

    out = out();
    out.write_float_array(x, offs, len);

    float[] r = new float[ x.length ];

    out.create_input_stream().read_float_array(r, offs, len);

    boolean eq = true;
    for (int i = offs; i < offs + len; i++)
      {
        if (x [ i ] != r [ i ])
          eq = false;
      }
    assertTrue("float array", eq);
  }

  public void testRead_long()
  {
    int expectedReturn = r.nextInt();
    out = out();
    out.write_long(expectedReturn);

    assertEquals("long", out.create_input_stream().read_long(), expectedReturn);
  }

  public void testRead_long_array()
  {
    int[] x = new int[ 24 ];
    for (int i = 0; i < x.length; i++)
      {
        x [ i ] = r.nextInt();
      }

    int offs = r.nextInt(5);
    int len = r.nextInt(10) + 1;

    out = out();
    out.write_long_array(x, offs, len);

    int[] r = new int[ x.length ];

    out.create_input_stream().read_long_array(r, offs, len);

    boolean eq = true;
    for (int i = offs; i < offs + len; i++)
      {
        if (x [ i ] != r [ i ])
          {
            eq = false;
            System.out.println(x [ i ] + "!=" + r [ i ] + " at " + i);
          }
      }
    assertTrue("long array", eq);
  }

  public void testRead_longlong()
  {
    long expectedReturn = r.nextLong();
    out = out();
    out.write_longlong(expectedReturn);

    assertEquals("long long", out.create_input_stream().read_longlong(),
                 expectedReturn
                );
  }

  public void testRead_longlong_array()
  {
    long[] x = new long[ 24 ];
    for (int i = 0; i < x.length; i++)
      {
        x [ i ] = r.nextLong();
      }

    int offs = r.nextInt(5);
    int len = r.nextInt(10) + 1;

    out = out();
    out.write_longlong_array(x, offs, len);

    long[] r = new long[ x.length ];

    out.create_input_stream().read_longlong_array(r, offs, len);

    boolean eq = true;
    for (int i = offs; i < offs + len; i++)
      {
        if (x [ i ] != r [ i ])
          eq = false;
      }
    assertTrue("long long array", eq);
  }

  public void testRead_octet()
  {
    byte expectedReturn =
      (byte) (r.nextInt(Byte.MAX_VALUE) - Byte.MAX_VALUE / 2);
    out = out();
    out.write_octet(expectedReturn);

    assertEquals("byte", out.create_input_stream().read_octet(), expectedReturn);
  }

  public void testRead_octet_array()
  {
    byte[] x = new byte[ 24 ];
    for (int i = 0; i < x.length; i++)
      {
        x [ i ] = (byte) (r.nextInt(Byte.MAX_VALUE) - Byte.MAX_VALUE / 2);
      }

    int offs = r.nextInt(5);
    int len = r.nextInt(10) + 1;

    out = out();
    out.write_octet_array(x, offs, len);

    byte[] r = new byte[ x.length ];

    out.create_input_stream().read_octet_array(r, offs, len);

    boolean eq = true;
    for (int i = offs; i < offs + len; i++)
      {
        if (x [ i ] != r [ i ])
          eq = false;
      }
    assertTrue("octet array", eq);
  }

  public void testRead_short()
  {
    short expectedReturn = (short) r.nextInt(Short.MAX_VALUE);
    out = out();
    out.write_short(expectedReturn);

    assertEquals("short", out.create_input_stream().read_short(), expectedReturn);
  }

  public void testRead_short_array()
  {
    short[] x = new short[ 24 ];
    for (int i = 0; i < x.length; i++)
      {
        x [ i ] = (short) r.nextInt(Short.MAX_VALUE);
      }

    int offs = r.nextInt(5);
    int len = r.nextInt(10) + 1;

    out = out();
    out.write_short_array(x, offs, len);

    short[] r = new short[ x.length ];

    out.create_input_stream().read_short_array(r, offs, len);

    boolean eq = true;
    for (int i = offs; i < offs + len; i++)
      {
        if (x [ i ] != r [ i ])
          eq = false;
      }
    assertTrue("short array", eq);
  }

  public void testRead_string()
  {
    String expectedReturn = "http://wwww.gnu.org";

    out = out();
    out.write_string(expectedReturn);

    String actualReturn = out.create_input_stream().read_string();
    assertEquals("string", expectedReturn, actualReturn);
  }

  public void testRead_ulong()
  {
    int expectedReturn = r.nextInt();
    out = out();
    out.write_ulong(expectedReturn);

    assertEquals("ulong", out.create_input_stream().read_ulong(), expectedReturn);
  }

  public void testRead_ulong_array()
  {
    int[] x = new int[ 24 ];
    for (int i = 0; i < x.length; i++)
      {
        x [ i ] = r.nextInt();
      }

    int offs = r.nextInt(5);
    int len = r.nextInt(10) + 1;

    out = out();
    out.write_ulong_array(x, offs, len);

    int[] r = new int[ x.length ];

    out.create_input_stream().read_ulong_array(r, offs, len);

    boolean eq = true;
    for (int i = offs; i < offs + len; i++)
      {
        if (x [ i ] != r [ i ])
          eq = false;
      }
    assertTrue("ulong array", eq);
  }

  public void testRead_ulonglong()
  {
    long expectedReturn = r.nextLong();
    out = out();
    out.write_ulonglong(expectedReturn);

    assertEquals("u long long", out.create_input_stream().read_ulonglong(),
                 expectedReturn
                );
  }

  public void testRead_ulonglong_array()
  {
    long[] x = new long[ 24 ];
    for (int i = 0; i < x.length; i++)
      {
        x [ i ] = r.nextLong();
      }

    int offs = r.nextInt(5);
    int len = r.nextInt(10) + 1;

    out = out();
    out.write_ulonglong_array(x, offs, len);

    long[] r = new long[ x.length ];

    out.create_input_stream().read_ulonglong_array(r, offs, len);

    boolean eq = true;
    for (int i = offs; i < offs + len; i++)
      {
        if (x [ i ] != r [ i ])
          eq = false;
      }
    assertTrue("u long long array", eq);
  }

  public void testRead_ushort()
  {
    short expectedReturn = (short) r.nextInt(Short.MAX_VALUE);
    out = out();
    out.write_ushort(expectedReturn);

    assertEquals("ushort", out.create_input_stream().read_ushort(),
                 expectedReturn
                );
  }

  public void testRead_ushort_array()
  {
    short[] x = new short[ 24 ];
    for (int i = 0; i < x.length; i++)
      {
        x [ i ] = (short) r.nextInt(Short.MAX_VALUE);
      }

    int offs = r.nextInt(5);
    int len = r.nextInt(10) + 1;

    out = out();
    out.write_ushort_array(x, offs, len);

    short[] r = new short[ x.length ];

    out.create_input_stream().read_ushort_array(r, offs, len);

    boolean eq = true;
    for (int i = offs; i < offs + len; i++)
      {
        if (x [ i ] != r [ i ])
          eq = false;
      }
    assertTrue("u short array", eq);
  }

  public void testRead_wchar()
  {
    out = out();
    out.write_wchar('\u017E');

    assertEquals("wide char", out.create_input_stream().read_wchar(), '\u017E');
  }

  public void testRead_wchar_array()
  {
    char[] x = new char[ 24 ];
    for (int i = 0; i < x.length; i++)
      {
        x [ i ] = r.nextBoolean() ? '\u0105' : '\u010D';
      }

    int offs = r.nextInt(5);
    int len = r.nextInt(10) + 1;

    out = out();
    out.write_wchar_array(x, offs, len);

    char[] r = new char[ x.length ];

    out.create_input_stream().read_wchar_array(r, offs, len);

    boolean eq = true;
    for (int i = offs; i < offs + len; i++)
      {
        if (x [ i ] != r [ i ])
          eq = false;
      }
    assertTrue("wide char array", eq);
  }

  public void testRead_wstring()
  {
    String expectedReturn = "Audrius Me\u0161kauskas";

    out = out();
    out.write_wstring(expectedReturn);

    String actualReturn = out.create_input_stream().read_wstring();
    assertEquals("wide string", expectedReturn, actualReturn);
  }

  org.omg.CORBA.portable.OutputStream out()
  {
    return orb.create_any().create_output_stream();
  }
}
