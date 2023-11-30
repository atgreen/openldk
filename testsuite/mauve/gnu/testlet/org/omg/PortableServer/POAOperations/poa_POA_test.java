// Tags: JDK1.4
// Uses: ../../CORBA/Asserter

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

package gnu.testlet.org.omg.PortableServer.POAOperations;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.Asserter;
import gnu.testlet.org.omg.PortableServer.POAOperations.communication.ourUserException;
import gnu.testlet.org.omg.PortableServer.POAOperations.communication.poa_Server;
import gnu.testlet.org.omg.PortableServer.POAOperations.communication.poa_comTester;
import gnu.testlet.org.omg.PortableServer.POAOperations.communication.poa_comTesterHelper;
import gnu.testlet.org.omg.PortableServer.POAOperations.communication.remotePoaControl;
import gnu.testlet.org.omg.PortableServer.POAOperations.communication.remotePoaControlHelper;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.OBJECT_NOT_EXIST;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TRANSIENT;
import org.omg.PortableServer.POAManagerPackage.State;

import java.util.HashSet;
import java.util.Iterator;

/**
 * This code controls the remote poa by turining it into various modes.
 * It operates two objects, one being the remote POA control servant
 * (connected to the root poa on a server side) and another the
 * test servant, connected to the poa being controlled. The poa being
 * controlled is a child for the root poa.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class poa_POA_test
  extends Asserter
  implements Testlet
{
  /*
  * The IOR.txt file, used to find the server and the object on the server. is written when starting the accompanying
  */
  public static final String ssTARGET_IOR_FILE0 = "IOR.txt";

  /**
   * The IOR for the object, connected to POA with the single servant policy.
   */
  public static final String ssTARGET_IOR_FILE1 = "ssIOR1.txt";
  public static final String ssTARGET_IOR_FILE2 = "ssIOR2.txt";
  public static final String ssTARGET_IOR_FILE3 = "ssIOR3.txt";
  public static final String[] allServants =
    new String[]
    {
      ssTARGET_IOR_FILE0, ssTARGET_IOR_FILE1, ssTARGET_IOR_FILE2,
      ssTARGET_IOR_FILE3
    };

  /*
  * The Control.txt file, used to find the server and the object on the server. is written when starting the accompanying
  */
  public static final String CONTROL_IOR_FILE = "Control.txt";
  ORB orb;

  /**
   * The control panel, managing the poa where
   * the main invocation target is connected.
   * The panel itself is connected to the target
   * parent POA.
   */
  remotePoaControl control;
  int holdPassed;
  int discarded;
  poa_Server server;
  poa_POA_test THIS = this;

  /**
   * THIS MUST BE THE FIRST TEST TO RUN after setUp!!!"
   * Test how many times various servants were activated when handling
   * a simple task 3 times.
   */
  public void test_RETAIN_Activation()
  {
    server.once_activated.incarnations.clear();
    server.once_activated.etherializations.clear();

    poa_comTester uobject =
      poa_comTesterHelper.narrow(readIOR(ssTARGET_IOR_FILE0, orb));
    uobject.sayHello();

    for (int j = 0; j < 3; j++)
      for (int i = 0; i < allServants.length; i++)
        {
          poa_comTester object =
            poa_comTesterHelper.narrow(readIOR(allServants [ i ], orb));
          String r = object.passCharacters("abba", "baba");
          assertEquals("wide/narrow strings", "return 'baba' and 'abba'", r);
        }

    Iterator iter = server.once_activated.incarnations.iterator();

    assertEquals("Must be activated once", 1,
                 server.once_activated.incarnations.size()
                );
    assertEquals("Must not be deactivated", 0,
                 server.once_activated.etherializations.size()
                );
    assertEquals("Activated object", "4 5 2 5 7 2 /1",
                 server.once_activated.incarnations.get(0)
                );

    // Ensure that all requests are served by the same servant.
    poa_comTester object =
      poa_comTesterHelper.narrow(readIOR(ssTARGET_IOR_FILE0, orb));

    String s = object.sayHello();
    assertTrue("Object key", s.startsWith("4 5 2 5 7 2 :"));

    String n;

    for (int i = 0; i < 10; i++)
      {
        n = object.sayHello();
        assertEquals("Must be same servant", s, n);
      }
  }

  public void testActivatedPoaAccess()
  {
    poa_comTester object =
      poa_comTesterHelper.narrow(readIOR("xactivated", orb));
  }

  /**
   * Get the object reference.
   */
  public void testPOA()
  {
    poa_comTester object;

    try
      {
        ORB orb = org.omg.CORBA.ORB.init(new String[ 0 ], null);

        object = poa_comTesterHelper.narrow(readIOR(ssTARGET_IOR_FILE0, orb));
        control = remotePoaControlHelper.narrow(readIOR(CONTROL_IOR_FILE, orb));

        object = poa_comTesterHelper.narrow(readIOR(ssTARGET_IOR_FILE1, orb));

        assertEquals("testPOA", 17, object.theField());

        object.sayHello();
        object = poa_comTesterHelper.narrow(readIOR(ssTARGET_IOR_FILE2, orb));
        object.sayHello();
      }
    catch (Throwable t)
      {
        t.printStackTrace();
        fail("" + t);
      }
  }

  public void test_NO_RETAIN()
  {
    // The POA of this object uses NO_RETAIN with servant locator.
    // That locator must be requested to find each time a new servant.
    poa_comTester object =
      poa_comTesterHelper.narrow(readIOR(ssTARGET_IOR_FILE3, orb));

    server.m575.preinvokes.clear();
    server.m575.postinvokes.clear();

    String s1 = object.sayHello();
    String s2 = object.sayHello();
    String s3 = object.sayHello();

    assertTrue("NO_RETAIN key", s1.startsWith("5 7 5 :"));
    assertTrue("NO_RETAIN key", s2.startsWith("5 7 5 :"));
    assertTrue("NO_RETAIN key", s3.startsWith("5 7 5 :"));

    assertEquals("NO_RETAIN, pre", 3, server.m575.preinvokes.size());
    assertEquals("NO_RETAIN, post", 3, server.m575.postinvokes.size());

    Iterator iter = server.m575.preinvokes.iterator();
    while (iter.hasNext())
      {
        String item = (String) iter.next();
        assertTrue("NO_RETAIN pre method/object",
                   item.startsWith("5 7 5 sayHello/sub")
                  );
      }

    iter = server.m575.postinvokes.iterator();

    // The sevant hash codes, from the sever side.
    HashSet servants = new HashSet();

    // The servant hash codes, from the client side (must be the same)
    HashSet servants2 = new HashSet();

    String hash;

    hash = s1.substring(s1.lastIndexOf(":"));
    servants2.add(hash);
    hash = s2.substring(s2.lastIndexOf(":"));
    servants2.add(hash);
    hash = s3.substring(s1.lastIndexOf(":"));
    servants2.add(hash);

    while (iter.hasNext())
      {
        String item = (String) iter.next();
        assertTrue("NO_RETAIN post method/object/cookie",
                   item.startsWith("5 7 5 sayHello/sub/cook")
                  );
        hash = item.substring(item.lastIndexOf(":"));
        servants.add(hash);
        assertTrue("NO_RETAIN servant hash", servants2.contains(hash));
      }
    assertEquals("NO_RETAIN all servants should differ", 3, servants.size());
  }

  /**
   * As all tests run on the same jre, the IORs are passed via static field.
   */
  public static org.omg.CORBA.Object readIOR(String file, ORB orb)
  {
    String ior = (String) poa_Server.references.get(file);
    return orb.string_to_object(ior);
  }

  public void testFieldAccess()
  {
    poa_comTester object =
      poa_comTesterHelper.narrow(readIOR(ssTARGET_IOR_FILE0, orb));

    object.theField(222);
    assertEquals("fieldAccess:1", 222, object.theField());
    object.theField(17);
    assertEquals("fieldAccess:2", 17, object.theField());
  }

  /**
   * This test works with POA that has both default servant and
   * retain policy. One object is activated with a separate servant.
   * The others two use the default servant.
   */
  public void testCombinedActivation()
  {
    poa_comTester t1 = poa_comTesterHelper.narrow(readIOR("T1", orb));
    poa_comTester t2 = poa_comTesterHelper.narrow(readIOR("T2", orb));
    poa_comTester tx = poa_comTesterHelper.narrow(readIOR("TX", orb));

    String s1 = t1.sayHello();
    String s2 = t2.sayHello();
    String sx = tx.sayHello();

    String h1 = s1.substring(s1.lastIndexOf(":"));
    String h2 = s2.substring(s2.lastIndexOf(":"));
    String hx = sx.substring(sx.lastIndexOf(":"));

    assertEquals("Must be same default servant", h1, h2);
    assertFalse("Must be different servant", h1.equals(hx));
    assertFalse("Must be different servant", h2.equals(hx));

    // Verify keys also.
    assertTrue("combinedActivation:1", s1.startsWith("4 4 4 1 :"));
    assertTrue("combinedActivation:2", s2.startsWith("4 4 4 2 :"));
    assertTrue("combinedActivation:3", sx.startsWith("4 4 4 5 5 5 5 :"));
  }

  public void testPOAControl()
  {
    poa_comTester object =
      poa_comTesterHelper.narrow(readIOR(ssTARGET_IOR_FILE0, orb));

    poa_comTester other_poa =
      poa_comTesterHelper.narrow(readIOR(ssTARGET_IOR_FILE1, orb));

    control = remotePoaControlHelper.narrow(readIOR(CONTROL_IOR_FILE, orb));

    try
      {
        object.sayHello();
      }
    catch (Exception ex)
      {
        fail("First invocation " + ex);
      }

    server.once_activated.incarnations.clear();
    server.once_activated.etherializations.clear();

    control.setControlTarget("1");
    control.setPoaMode(State._DISCARDING);

    try
      {
        object.sayHello();
        fail("Expected throwing TRANSIENT, minor 1");
      }
    catch (TRANSIENT ex)
      {
        // OK.
      }
    catch (Exception other)
      {
        fail("Expected TRANSIENT, not " + other);
      }

    try
      {
        other_poa.sayHello();
        try
          {
            other_poa.throwException(555);
            fail("Must throw exception");
          }
        catch (ourUserException ex)
          {
            assertEquals("Exception code", 555, ex.ourField);
          }

        // This must completely pass.
        test_NO_RETAIN();
      }
    catch (Exception ex)
      {
        fail("Other POA must still work. " + ex);
      }

    // Activate it again.
    control.setPoaMode(State._ACTIVE);

    try
      {
        // Now active again and must work.
        object.sayHello();
      }
    catch (Exception ex)
      {
        fail("Reactivation from Discarding " + ex);
      }

    // Activate POA
    control.setPoaMode(State._ACTIVE);

    // Deactivate an object.
    control.setPoaMode(100);
    try
      {
        object.sayHello();
      }
    catch (OBJECT_NOT_EXIST ex)
      {
        fail("Must be implicitly activated ");
      }

    // Check for etherializations and incarnations.
    assertEquals("One incarnation", server.once_activated.incarnations.size(), 1);
    assertEquals("One etherialization",
                 server.once_activated.etherializations.size(), 1
                );

    assertEquals("Incarnation", "4 5 2 5 7 2 /1",
                 server.once_activated.incarnations.get(0)
                );
    assertEquals("Etherialization", "4 5 2 5 7 2 /1",
                 server.once_activated.etherializations.get(0)
                );
  }

  public void testExceptions()
  {
    for (int i = 0; i < allServants.length; i++)
      {
        poa_comTester object =
          poa_comTesterHelper.narrow(readIOR(allServants [ i ], orb));
        try
          {
            object.throwException(64);
            fail("User exception is not thrown");
          }
        catch (ourUserException ex)
          {
            assertEquals("Wrong field in user exception.", 64, ex.ourField);
          }

        try
          {
            object.throwException(-1);
            fail("System exception is not thrown");
          }
        catch (BAD_OPERATION ex)
          {
            assertEquals("SysEx minor code", 456, ex.minor);
          }
        catch (Exception ex)
          {
            fail("Throwing incorrect exception " + ex);
          }
      }
  }

  public void testRedirectionWithActivator()
  {
    poa_comTester r =
      poa_comTesterHelper.narrow(readIOR("willRedirActivator.txt", orb));

    // Ensure the repetetive calls are also redirected.
    for (int i = 0; i < 5; i++)
      {
        String s = r.sayHello();
        assertEquals("Redir with activator", "{redirection handler} 7 7 7 ", s);
      }
  }

  public void testRedirectionWithLocator()
  {
    poa_comTester r =
      poa_comTesterHelper.narrow(readIOR("willRedirLocator.txt", orb));

    // Ensure the repetetive calls are also redirected.
    for (int i = 0; i < 5; i++)
      {
        String s = r.sayHello();
        assertEquals("Redir with locator", "{redirection handler} 7 7 7 ", s);
      }
  }

  protected void setUp() throws java.lang.Exception
  {
    server = new poa_Server();
    server.start_server(THIS.h);

    try
      {
        // Give 500 ms for the server thread to start.
        Thread.sleep(500);
      }
    catch (InterruptedException ex)
      {
      }
    orb = org.omg.CORBA.ORB.init(new String[0], null);
  }

  public void test(TestHarness a_harness)
  {
    h = a_harness;
    try
      {
        setUp();
        test_NO_RETAIN();
        test_RETAIN_Activation();
        testActivatedPoaAccess();
        testCombinedActivation();
        testExceptions();
        testFieldAccess();
        testPOA();
        testPOAControl();
        testRedirectionWithActivator();
        testRedirectionWithLocator();
        orb.destroy();
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        h.fail("Exc:" + ex + ":" + ex.getCause());
      }
  }
}