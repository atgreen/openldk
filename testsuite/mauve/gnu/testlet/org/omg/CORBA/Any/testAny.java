// Tags: JDK1.2
// Uses: ../Asserter

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


package gnu.testlet.org.omg.CORBA.Any;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.Asserter;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_INV_ORDER;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ORB;
import org.omg.CORBA.ShortHolder;
import org.omg.CORBA.ShortSeqHolder;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.Streamable;

import java.math.BigDecimal;

import java.util.Random;

/**
 * Test the CORBA Any. The actual class being tested is obtained from
 * the ORB and depends from the implementation; in GNU Classpath it is
 * gnu.CORBA.gnuAny.
 *
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public class testAny
  extends Asserter
  implements Testlet
{
  private Any any;
  private ORB orb;
  private Random r = new Random();

  public void test(TestHarness harness)
  {
    h = harness;

    orb = ORB.init();
    any = orb.create_any();

    testEquals();
    testExtract_any();
    testExtract_boolean();
    testExtract_char();
    testExtract_double();
    testExtract_fixed();
    testExtract_float();
    testExtract_long();
    testExtract_longlong();
    testExtract_octet();
    testExtract_short();
    testExtract_Streamable();
    testExtract_string();
    testExtract_TypeCode();
    testExtract_ulong();
    testExtract_ulonglong();
    testExtract_ushort();
    testExtract_wchar();
    testExtract_wstring();
  }

  public void testEquals()
  {
    Any other = orb.create_any();
    other.insert_string("other");
    any.insert_string("this");

    assertFalse("eq1", any.equal(other));

    any.insert_string("other");
    assertTrue("eq2", any.equal(other));

    any.insert_long(1);
    assertFalse("eq3", any.equal(other));
  }

  public void testExtract_Streamable()
                              throws BAD_INV_ORDER
  {
    Streamable expectedReturn =
      new ShortHolder((short) r.nextInt(Short.MAX_VALUE));
    any.insert_Streamable(expectedReturn);

    Streamable actualReturn = any.extract_Streamable();
    assertEquals("Streamable", expectedReturn, actualReturn);
  }

  public void testExtract_TypeCode()
                            throws BAD_OPERATION
  {
    TypeCode expectedReturn = new ShortSeqHolder(new short[ 0 ])._type();
    any.insert_TypeCode(expectedReturn);

    TypeCode actualReturn = any.extract_TypeCode();
    assertEquals("typecode", expectedReturn, actualReturn);
  }

  public void testExtract_any()
                       throws BAD_OPERATION
  {
    Any expectedReturn = orb.create_any();
    expectedReturn.insert_longlong(r.nextLong());

    any.insert_any(expectedReturn);

    Any actualReturn = any.extract_any();
    assertEquals("Any inside Any", expectedReturn, actualReturn);
  }

  public void testExtract_boolean()
                           throws BAD_OPERATION
  {
    boolean expectedReturn = false;
    any.insert_boolean(expectedReturn);

    boolean actualReturn = any.extract_boolean();
    assertEquals("boolean", expectedReturn, actualReturn);

    expectedReturn = true;
    any.insert_boolean(expectedReturn);
    actualReturn = any.extract_boolean();
    assertEquals("boolean", expectedReturn, actualReturn);
  }

  public void testExtract_char()
                        throws BAD_OPERATION
  {
    char expectedReturn = 'z';
    any.insert_char(expectedReturn);

    char actualReturn = any.extract_char();
    assertEquals("char", expectedReturn, actualReturn);
  }

  public void testExtract_double()
                          throws BAD_OPERATION
  {
    double expectedReturn = r.nextDouble();
    any.insert_double(expectedReturn);

    double actualReturn = any.extract_double();
    assertEquals("double", expectedReturn, actualReturn, Double.MIN_VALUE);
  }

  public void testExtract_fixed()
                         throws BAD_OPERATION
  {
    BigDecimal expectedReturn = new BigDecimal("123.456");
    any.insert_fixed(expectedReturn,
                     orb.create_fixed_tc((short) 6,
                                         (short) expectedReturn.scale()
                                        )
                    );

    BigDecimal actualReturn = any.extract_fixed();
    assertEquals("fixed", expectedReturn, actualReturn);
  }

  public void testExtract_float()
                         throws BAD_OPERATION
  {
    float expectedReturn = r.nextFloat();
    any.insert_float(expectedReturn);

    float actualReturn = any.extract_float();
    assertEquals("float", expectedReturn, actualReturn, Float.MIN_VALUE);
  }

  public void testExtract_long()
                        throws BAD_OPERATION
  {
    int expectedReturn = r.nextInt() - Integer.MAX_VALUE / 2;
    any.insert_long(expectedReturn);

    int actualReturn = any.extract_long();
    assertEquals("long", expectedReturn, actualReturn);
  }

  public void testExtract_longlong()
                            throws BAD_OPERATION
  {
    long expectedReturn = r.nextLong() - (Long.MAX_VALUE / 2);
    any.insert_longlong(expectedReturn);

    long actualReturn = any.extract_longlong();
    assertEquals("long", expectedReturn, actualReturn);
  }

  public void testExtract_octet()
                         throws BAD_OPERATION
  {
    byte expectedReturn = (byte) r.nextInt(Byte.MAX_VALUE);
    any.insert_octet(expectedReturn);

    byte actualReturn = any.extract_octet();
    assertEquals("byte (octet)", expectedReturn, actualReturn);
  }

  public void testExtract_short()
                         throws BAD_OPERATION
  {
    short expectedReturn =
      (short) (r.nextInt(Short.MAX_VALUE) - Short.MAX_VALUE / 2);

    any.insert_short(expectedReturn);

    short actualReturn = any.extract_short();
    assertEquals("short", expectedReturn, actualReturn);
  }

  public void testExtract_string()
                          throws BAD_OPERATION
  {
    String expectedReturn = "http://www.lithuania.lt";
    any.insert_string(expectedReturn);

    String actualReturn = any.extract_string();
    assertEquals("string", expectedReturn, actualReturn);
  }

  public void testExtract_ulong()
                         throws BAD_OPERATION
  {
    int expectedReturn = r.nextInt(Integer.MAX_VALUE);
    any.insert_ulong(expectedReturn);

    int actualReturn = any.extract_ulong();
    assertEquals("unsigned long", expectedReturn, actualReturn);
  }

  public void testExtract_ulonglong()
                             throws BAD_OPERATION
  {
    long expectedReturn = Math.abs(r.nextLong());
    any.insert_ulonglong(expectedReturn);

    long actualReturn = any.extract_ulonglong();
    assertEquals("unsigned long long", expectedReturn, actualReturn);
  }

  public void testExtract_ushort()
                          throws BAD_OPERATION
  {
    short expectedReturn = (short) r.nextInt(Short.MAX_VALUE);
    any.insert_ushort(expectedReturn);

    short actualReturn = any.extract_ushort();
    assertEquals("unsigned short", expectedReturn, actualReturn);
  }

  public void testExtract_wchar()
                         throws BAD_OPERATION
  {
    char expectedReturn = '\u017E';
    any.insert_wchar(expectedReturn);

    char actualReturn = any.extract_wchar();
    assertEquals("wchar", expectedReturn, actualReturn);
  }

  public void testExtract_wstring()
                           throws BAD_OPERATION
  {
    String expectedReturn =
      "http://www.lithuania.lt and \u0105\u010D\u0119\u0117" +
      "\u012F\u0161\u0173\u016B\u017E\u0104\u0118.";
    any.insert_wstring(expectedReturn);

    String actualReturn = any.extract_wstring();
    assertEquals("wstring", expectedReturn, actualReturn);
  }
}
