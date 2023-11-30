// Tags: JDK1.4
// Uses: ../../CORBA/Asserter ../../PortableServer/POAOperations/communication/ourUserException ../../PortableServer/POAOperations/communication/poa_Servant ../../PortableServer/POAOperations/communication/poa_comTester ../../PortableServer/POAOperations/communication/poa_comTesterHelper

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA.


package gnu.testlet.org.omg.PortableInterceptor.Interceptor;

import java.util.Properties;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.INV_FLAG;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.Request;
import org.omg.CORBA.TCKind;
import org.omg.PortableInterceptor.Current;
import org.omg.PortableInterceptor.CurrentHelper;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.Asserter;
import gnu.testlet.org.omg.PortableServer.POAOperations.communication.ourUserException;
import gnu.testlet.org.omg.PortableServer.POAOperations.communication.poa_Servant;
import gnu.testlet.org.omg.PortableServer.POAOperations.communication.poa_comTester;
import gnu.testlet.org.omg.PortableServer.POAOperations.communication.poa_comTesterHelper;

/**
 * Test the basic work of the portable interceptor system.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class testInterceptors extends Asserter implements Testlet
{
  public static Object fior;

  public void test()
  {
    try
      {
        Properties initialisers = new Properties();
        initialisers.setProperty(
          "org.omg.PortableInterceptor.ORBInitializerClass." +
          ucInitialiser.class.getName(),
          ucInitialiser.class.getName()
        );

        // Create and initialize the ORB
        final ORB orb = org.omg.CORBA.ORB.init(new String[ 0 ], initialisers);

        assertTrue("PreInit", ucInitialiser.preInit);
        assertTrue("PostInit", ucInitialiser.postInit);

        // Create the general communication servant and register it
        // with the ORB
        poa_Servant tester = new poa_Servant();

        POA rootPOA =
          POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
        Object object = rootPOA.servant_to_reference(tester);

        // IOR must contain custom fragment, inserted by interceptor.
        // Sun 1.4 had a bug that was fixed in 1.5.
        String ior = orb.object_to_string(object);

        assertTrue("IOR custom component (bug in 1.4, fixed in 1.5)",
          ior.indexOf(
            "45257200000020000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
          ) > 0
        );

        // Create the forwarding target and register it
        // with the ORB
        poa_Servant forw = new poa_Servant();

        tester.theField(15);
        forw.theField(16);

        // Another orb without interceptors.
        final ORB orbf = ORB.init(new String[ 0 ], null);

        POA rootPOA2 =
          POAHelper.narrow(orbf.resolve_initial_references("RootPOA"));

        Object fobject = rootPOA2.servant_to_reference(forw);

        // Storing the IOR reference for general communication.
        fior = fobject;

        rootPOA.the_POAManager().activate();
        rootPOA2.the_POAManager().activate();

        // Intercepting server ready and waiting ...
        new Thread()
          {
            public void run()
            {
              // wait for invocations from clients
              orb.run();
            }
          }.start();

        new Thread()
          {
            public void run()
            {
              // wait for invocations from clients
              orbf.run();
            }
          }.start();

        // Make pause and do a local call.
        Thread.sleep(1000);

        // Saying local hello.
        poa_comTester Tester = poa_comTesterHelper.narrow(object);

        Any a0 = orb.create_any();
        a0.insert_string("Initial value for slot 0");

        Any a1 = orb.create_any();
        a1.insert_string("Initial value for slot 1");

        ORB orb2 = ORB.init(new String[ 0 ], initialisers);

        try
          {
            // Set the initial slot values.
            Current current =
              CurrentHelper.narrow(orb.resolve_initial_references("PICurrent"));
            Current current2 =
              CurrentHelper.narrow(orb2.resolve_initial_references("PICurrent"));

            current.set_slot(ucInitialiser.slot_0, a0);
            current.set_slot(ucInitialiser.slot_1, a1);

            current2.set_slot(ucInitialiser.slot_0, a0);
            current2.set_slot(ucInitialiser.slot_1, a1);
          }
        catch (Exception e)
          {
            fail("Exception " + e + " while setting slots.");
            e.printStackTrace();
          }

        String lHello = Tester.sayHello();

        // Saying remote hello.
        Object object2 = orb2.string_to_object(ior);

        poa_comTester Tester2 = poa_comTesterHelper.narrow(object2);

        String hello = Tester2.sayHello();

        assertEquals("Local and remote must return the same", lHello, hello);

        // Saying remote hello via DII.
        Request rq =
          Tester2._create_request(null, "sayHello", orb2.create_list(0),
            orb2.create_named_value("", orb.create_any(), 0)
          );
        rq.set_return_type(orb2.get_primitive_tc(TCKind.tk_string));

        rq.invoke();

        assertEquals("Remote Stub and DII call must return the same", hello,
          rq.return_value().extract_string()
        );

        // Saying local hello via DII.
        rq =
          Tester._create_request(null, "sayHello", orb2.create_list(0),
            orb2.create_named_value("", orb.create_any(), 0)
          );
        rq.set_return_type(orb2.get_primitive_tc(TCKind.tk_string));

        rq.invoke();

        assertEquals("Local Stub and DII call must return the same", hello,
          rq.return_value().extract_string()
        );

        // Throw remote system exception
        try
          {
            Tester2.throwException(-1);
            fail("BAD_OPERATION should be thrown");
          }
        catch (BAD_OPERATION ex)
          {
            assertEquals("Minor code", ex.minor, 456);
            assertEquals("Completion status", CompletionStatus.COMPLETED_YES,
              ex.completed
            );
          }

        // Throw remote user exception
        try
          {
            Tester2.throwException(24);
            fail("UserException should be thrown");
          }
        catch (ourUserException ex)
          {
            assertEquals("Custom field", ex.ourField, 24);
          }

        // Throw local system exception
        try
          {
            Tester.throwException(-1);
            fail("BAD_OPERATION should be thrown");
          }
        catch (BAD_OPERATION ex)
          {
            assertEquals("Minor code", ex.minor, 456);
            assertEquals("Completion status", CompletionStatus.COMPLETED_YES,
              ex.completed
            );
          }

        // Throw local user exception
        try
          {
            Tester.throwException(24);
            fail("UserException should be thrown");
          }
        catch (ourUserException ex)
          {
            assertEquals("Custom field", ex.ourField, 24);
          }

        // Remote server side interceptor throws an exception
        try
          {
            Tester2.passCharacters("", "");
            fail("INV_FLAG should be thrown");
          }
        catch (INV_FLAG ex)
          {
            assertEquals("Minor", 52, ex.minor);
            assertEquals("Completion status",
              CompletionStatus.COMPLETED_MAYBE, ex.completed
            );
          }

        // Local server side interceptor throws an exception
        try
          {
            Tester.passCharacters("", "");
            fail("INV_FLAG should be thrown");
          }
        catch (INV_FLAG ex)
          {
            assertEquals("Minor", 52, ex.minor);
            assertEquals("Completion status",
              CompletionStatus.COMPLETED_MAYBE, ex.completed
            );
          }

        assertEquals("Forwarding test, remote", 16, Tester2.theField());
        assertEquals("Forwarding test, local", 16, Tester.theField());

        assertEquals("Forwarding test, remote", 16, Tester2.theField());
        assertEquals("Forwarding test, local", 16, Tester.theField());

        // Destroy orbs:
        orb.destroy();
        orb2.destroy();
        orbf.destroy();

        assertTrue("Destroyed", ucClientRequestInterceptor.destroyed);
        assertTrue("Destroyed", ucIorInterceptor.destroyed);
        assertTrue("Destroyed", ucServerRequestInterceptor.destroyed);
      }
    catch (Exception e)
      {
        fail("Exception " + e);
      }
  }

  public void test(TestHarness harness)
  {
    // Set the loader of this class as a context class loader, ensuring that the
    // CORBA implementation will be able to locate the interceptor classes.
    ClassLoader previous = Thread.currentThread().getContextClassLoader();
    Thread.currentThread().setContextClassLoader(getClass().getClassLoader());
    
    try
      {
        h = harness;
        test();
      }
    finally
      {
        Thread.currentThread().setContextClassLoader(previous);
      }
  }
}