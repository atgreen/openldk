// Tags: JDK1.2
/* RequestTest.java --
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
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.NVList;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Request;
import org.omg.CORBA.ShortHolder;
import org.omg.CORBA.StringHolder;
import org.omg.CORBA.TCKind;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.Asserter;
import gnu.testlet.org.omg.CORBA.ORB.communication.node;
import gnu.testlet.org.omg.CORBA.ORB.communication.ourUserExceptionHelper;

/**
 * Test invocations using org.omg.Request.
 *
 * Warning: this test start CORBA server on port 1126. Be sure your
 * security restrictions allow that server to start.
 *
 * This Classpath example was modified, converting it into the test.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class RequestTest
  extends Asserter
  implements Testlet
{
  ORB orb;
  org.omg.CORBA.Object object;

  public void test(TestHarness harness)
  {
    try
      {
        setUp();
      }
    catch (Exception ex)
      {
        harness.fail("Unexpected " + ex.getClass().getName() + ":" +
                     ex.getMessage() + " at setup."
                    );
      }

    h = harness;

    testParameters();
    testSystemException();
    testWideNarrowStrings();
  }

  public void testParameters()
  {
    Request r =
      object._create_request(null, "passSimple", orb.create_list(0), null);

    ByteHolder a_byte = new ByteHolder((byte) 0);
    ShortHolder a_short = new ShortHolder((short) 3);
    StringHolder a_string = new StringHolder("[string 4]");

    // This is an 'out' parameter; the value must not be passed to servant.
    DoubleHolder a_double = new DoubleHolder(56.789);

    r.add_inout_arg().insert_octet((byte) 0);
    r.add_in_arg().insert_long(2);
    r.add_inout_arg().insert_short((short) 3);
    r.add_inout_arg().insert_string("[string 4]");
    r.add_out_arg().type(orb.get_primitive_tc(TCKind.tk_double));

    NVList para = r.arguments();

    try
      {
        assertEquals("octet", para.item(0).value().extract_octet(), 0);
        assertEquals("long (in parameter)",
                     para.item(1).value().extract_long(), 2
                    );
        assertEquals("short", para.item(2).value().extract_short(), 3);
        assertEquals("string", para.item(3).value().extract_string(),
                     "[string 4]"
                    );
      }
    catch (Exception ex)
      {
        fail("Unexpected " + ex.getClass().getName() + ":" + ex.getMessage());
      }

    // For the last parameter, the value is not set.
    r.set_return_type(orb.get_primitive_tc(TCKind.tk_long));

    r.invoke();

    assertEquals("Returned value", r.result().value().extract_long(), 452572);

    para = r.arguments();

    try
      {
        assertEquals("octet", para.item(0).value().extract_octet(), 1);
        assertEquals("long (in parameter)",
                     para.item(1).value().extract_long(), 2
                    );
        assertEquals("short", para.item(2).value().extract_short(), 4);
        assertEquals("string", para.item(3).value().extract_string(),
                     "[string 4] [return]"
                    );
        assertEquals("double", para.item(4).value().extract_double(), 1.0,
                     Double.MIN_VALUE
                    );
      }
    catch (Exception ex)
      {
        fail("Unexpected " + ex.getClass().getName() + ":" + ex.getMessage());
      }
  }

  public void testSystemException()
  {
    try
      {
        ExceptionList exList = orb.create_exception_list();
        exList.add(ourUserExceptionHelper.type());

        Request rq =
          object._create_request(null, "throwException", orb.create_list(1),
                                 null, exList, null
                                );

        rq.add_in_arg().insert_long(-55);

        rq.invoke();

        fail("The BAD_OPERATION is not thrown");
      }
    catch (BAD_OPERATION ex)
      {
        assertEquals("Minor code", ex.minor, 456);
      }
  }

  public void testWideNarrowStrings()
                             throws BAD_OPERATION
  {
    Request rq =
      object._create_request(null, "passCharacters", orb.create_list(0), null);

    rq.add_in_arg().insert_wstring("wide string");
    rq.add_in_arg().insert_string("narrow string");

    rq.set_return_type(orb.get_primitive_tc(TCKind.tk_wstring));

    rq.invoke();

    assertEquals("Returned value", rq.result().value().extract_wstring(),
                 "return 'narrow string' and 'wide string'"
                );
  }

  protected void setUp()
                throws java.lang.Exception
  {
    String ior = comServer.start_server(new String[ 0 ])[0];

    orb = org.omg.CORBA.ORB.init(new String[ 0 ], null);
    object = orb.string_to_object(ior);
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
