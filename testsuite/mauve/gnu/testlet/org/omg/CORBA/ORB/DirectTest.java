// Tags: JDK1.2
// Uses: ../Asserter

/* DirectTest.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.testlet.org.omg.CORBA.ORB;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ByteHolder;
import org.omg.CORBA.DoubleHolder;
import org.omg.CORBA.ORB;
import org.omg.CORBA.ShortHolder;
import org.omg.CORBA.StringHolder;
import org.omg.CORBA.UserException;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.Asserter;
import gnu.testlet.org.omg.CORBA.ORB.communication.comTester;
import gnu.testlet.org.omg.CORBA.ORB.communication.node;
import gnu.testlet.org.omg.CORBA.ORB.communication.nodeHolder;
import gnu.testlet.org.omg.CORBA.ORB.communication.ourUserException;
import gnu.testlet.org.omg.CORBA.ORB.communication.passThis;
import gnu.testlet.org.omg.CORBA.ORB.communication.returnThis;

/**
 * Test the invocations by direct call after casting to an interface. Warning:
 * this test start CORBA server on port 1126. Be sure your security restrictions
 * allow that server to start. This Classpath example was modified, converting
 * it into the test.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class DirectTest
  extends Asserter
  implements Testlet
{
  ORB orb;
  ORB server;
  comTester object;

  public void test(TestHarness harness)
  {
    try
      {
        setUp();
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        harness.fail(ex.getClass().getName() + " in setup.");
      }

    h = harness;

    testField();
    testParameters();
    testStringArray();
    testStructure();
    testSystemException();
    testTree();
    testUserException();
    testWideNarrowStrings();
  }

  /**
   * Test the field getter/setter.
   */
  public void testField()
  {
    int def = object.theField();

    assertEquals("Initial value", def, 17);

    object.theField(55);

    int changed = object.theField();

    assertEquals("Changed value", changed, 55);

    // Put the old value back, allowing to re-run the test.
    object.theField(17);
  }

  public void testParameters()
  {
    ByteHolder a_byte = new ByteHolder((byte) 0);
    ShortHolder a_short = new ShortHolder((short) 3);
    StringHolder a_string = new StringHolder("[string 4]");

    // This is an 'out' parameter; the value must not be passed to servant.
    DoubleHolder a_double = new DoubleHolder(56.789);

    int returned = object.passSimple(a_byte, 2, a_short, a_string, a_double);

    assertEquals("Returned value", returned, 452572);

    assertEquals("octet", a_byte.value, 1);
    assertEquals("short", a_short.value, 4);
    assertEquals("string", a_string.value, "[string 4] [return]");
    assertEquals("double", a_double.value, 1.0, Double.MIN_VALUE);
  }

  public void testStringArray()
  {
    String[] x = new String[] { "one", "two" };
    String[] y = object.passStrings(x);

    for (int i = 0; i < y.length; i++)
      {
        assertEquals("string[]", y [ i ], x [ i ] + ":" + x [ i ]);
      }
  }

  public void testStructure()
  {
    passThis arg = new passThis();
    arg.a = "A";
    arg.b = "B";

    returnThis r = object.passStructure(arg);

    assertEquals("struct, string field", r.c, "AB");
    assertEquals("struct, int field", r.n, 555);

    assertEquals("array", r.arra [ 0 ], 11);
    assertEquals("array", r.arra [ 1 ], 22);
    assertEquals("array", r.arra [ 2 ], 33);
  }

  public void testSystemException()
  {
    try
      {
        object.throwException(-55);
        fail("The BAD_OPERATION is not thrown");
      }
    catch (BAD_OPERATION ex)
      {
        assertEquals("Minor code", ex.minor, 456);
      }
    catch (UserException uex)
      {
        fail("User exception must not be thrown");
      }
  }

  public void testTree()
  {
    node n = nod("Root");

    n.children = new node[] { nod("a"), nod("b") };
    n.children [ 1 ].children = new node[] { nod("ba"), nod("bb") };
    n.children [ 1 ].children [ 0 ].children = new node[] { nod("bac") };

    nodeHolder nh = new nodeHolder(n);

    object.passTree(nh);

    // Convert the returned tree to some strig representation.
    StringBuffer img = new StringBuffer();
    getImage(img, nh.value);

    assertEquals("Tree image",
                 "Root++: (a++: ()  b++: (ba++: (bac++: ()  )  bb++: ()  )  ) ",
                 img.toString()
                );
  }

  public void testUserException()
  {
    try
      {
        object.throwException(123);
        fail("The user exception is not thrown");
      }
    catch (ourUserException uex)
      {
        assertEquals("User exception arg", uex.ourField, 123);
      }
  }

  public void testWideNarrowStrings()
                             throws BAD_OPERATION
  {
    String r = object.passCharacters("wide string", "narrow string");
    assertEquals("Returned value", r, "return 'narrow string' and 'wide string'");
  }

  protected void setUp()
                throws java.lang.Exception
  {
    String ior = comServer.start_server(new String[ 0 ])[0];

    orb = org.omg.CORBA.ORB.init(new String[ 0 ], null);
    object = (comTester) orb.string_to_object(ior);
  }

  private void getImage(StringBuffer b, node n)
  {
    b.append(n.name);
    b.append(": (");

    for (int i = 0; i < n.children.length; i++)
      {
        getImage(b, n.children [ i ]);
        b.append(' ');
      }
    b.append(") ");
  }

  private node nod(String hdr)
  {
    node n = new node();
    n.children = new node[ 0 ];
    n.name = hdr;

    return n;
  }
}
