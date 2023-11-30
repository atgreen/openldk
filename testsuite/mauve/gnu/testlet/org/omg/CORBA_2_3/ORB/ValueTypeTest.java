// Tags: JDK1.3

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


package gnu.testlet.org.omg.CORBA_2_3.ORB;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.Asserter;
import gnu.testlet.org.omg.CORBA.ORB.comServer;
import gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype.*;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;

import java.io.InputStream;

/**
 * Tests the CORBA 2_3 features, related to the Value type,
 * introduced since 1.3.
 *
 * The test is formally possible since v 1.3. However some Sun
 * bugs are only fixed since 1.4.2_08_b03 inclusive, so the it
 * will not succeed with the earlier releases.

 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public class ValueTypeTest
  extends Asserter
  implements Testlet
{
  ORB orb;
  Greetings object;

  public void testCustomValue()
  {
    h.checkPoint("Custom value");

    ORB orb = ORB.init(new String[ 0 ], null);

    Any sc = orb.create_any();

    org.omg.CORBA_2_3.portable.OutputStream out =
      (org.omg.CORBA_2_3.portable.OutputStream) sc.create_output_stream();

    cmInfoImpl orig = new cmInfoImpl("first", "second");

    out.write_value(orig);

    Any b = orb.create_any();

    InputStream ou = out.create_input_stream();

    b.read_value((org.omg.CORBA_2_3.portable.InputStream) ou,
                 cmInfoHelper.type()
                );

    cmInfo s = (cmInfo) b.extract_Value();

    assertEquals("After Any, cv ", s.message, orig.message);
    assertEquals("After Any, cv ", s.name, orig.name);

    cmInfo a =
      (cmInfo) ((org.omg.CORBA_2_3.portable.InputStream) out.create_input_stream()).read_value();

    assertEquals("After stream, cv ", a.message, orig.message);
    assertEquals("After stream, cv ", a.name, orig.name);
  }

  public void testStreamableValue()
  {
    h.checkPoint("Streamable value");

    try
      {
        ORB orb = ORB.init(new String[ 0 ], null);

        for (int holder_mode = 0; holder_mode < 3; holder_mode++)
          {
            for (int helper_mode = 0; helper_mode < 4; helper_mode++)
              {
                InfoHolder.testMode = holder_mode;
                InfoHelper.testMode = helper_mode;

                String mode = holder_mode + ":" + helper_mode;

                Any sc = orb.create_any();

                org.omg.CORBA_2_3.portable.OutputStream out =
                  (org.omg.CORBA_2_3.portable.OutputStream) sc.create_output_stream();

                InfoImpl orig = new InfoImpl("first", "second");

                out.write_value(orig);

                Any b = orb.create_any();

                InputStream ou = out.create_input_stream();
                TypeCode type = orig._type();

                b.read_value((org.omg.CORBA_2_3.portable.InputStream) ou, type);

                Info s = (Info) b.extract_Value();

                assertEquals("After Any, sv " + mode, s._message, orig._message);
                assertEquals("After Any, sv " + mode, s._name, orig._name);

                Info a =
                  (Info) ((org.omg.CORBA_2_3.portable.InputStream) out.create_input_stream()).read_value();

                assertEquals("After stream, sv " + mode, a._message,
                             orig._message
                            );
                assertEquals("After stream, sv " + mode, a._name, orig._name);
              }
          }
      }
    catch (Exception ex)
      {
        fail(ex + ", Sun fixed this in 1.4.2 only.");
      }
  }

  public void testDirectComunication()
  {
    try
      {
        h.checkPoint("Value type transfer");

        InfoImpl info =
          new InfoImpl("http://www.gnu.org/software/classpath/classpath.html",
                       "http://www.lietuva.lt/"
                      );
        cmInfoImpl cinfo =
          new cmInfoImpl("http://www.akl.lt/en", "http://www.ffii.org/");

        InfoHolder h = new InfoHolder(info);
        cmInfoHolder ch = new cmInfoHolder(cinfo);

        object.hello(ch, h);

        assertEquals("Custom marshal ",
                     "http://www.akl.lt/en+;Names: " +
                     "http://www.akl.lt/en+http://www.gnu.org/software/" +
                     "classpath/classpath.html", ch.value.toString()
                    );

        assertEquals("Stramable value ",
                     "http://www.gnu.org/software/classpath/classpath.html" +
                     "+--Messages: http://www.lietuva.lt/+Names: " +
                     "http://www.akl.lt/en+http://www.gnu.org/software/" +
                     "classpath/classpath.html", h.value.toString()
                    );
      }
    catch (Exception ex)
      {
        fail(ex + ", Sun fixed this in 1.4.2 only.");
      }
  }

  protected void setUp()
  {
    String ior = comServer.start_server(new String[ 0 ]) [ 1 ];
    orb = org.omg.CORBA.ORB.init(new String[ 0 ], null);
    object = (Greetings) orb.string_to_object(ior);
  }

  public void test(TestHarness harness)
  {
    h = harness;
    // Set the loader of this class as a context class loader, ensuring that the
    // CORBA implementation will be able to locate the stub classes.
    ClassLoader previous = Thread.currentThread().getContextClassLoader();
    Thread.currentThread().setContextClassLoader(getClass().getClassLoader());
    try
      {
        setUp();
        testCustomValue();
        testStreamableValue();
        testDirectComunication();
      }
    catch (Throwable ex)
      {
        h.fail("Exception " + ex);
      }
    finally
      {
        Thread.currentThread().setContextClassLoader(previous);
      }
  }
}