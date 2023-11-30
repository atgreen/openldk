// Tags: JDK1.4

// Copyright (c) Object Oriented Concepts, Inc. Billerica, MA, USA

// Adapted for Mauve by Audrius Meskauskas <audriusa@bluewin.ch>

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

/*
This code originally came from the OMG's CORBA Open Source Testing project,
which lived at cost.omg.org. That site no longer exists.

All the contributing companies agreed to release their tests under the
terms of the GNU Lesser General Public License, available in the file
COPYING.LIB.

The code has been modified integrating into Mauve test environment and
removing tests that are not yet supported by Suns jre 1.4. Hence the license
is now GPL.

We downloaded the code from http://sourceforge.net/projects/corba-cost/,
administrated by Duncan Grisby.
*/


// **********************************************************************
//
// Copyright (c) 2000
// Object Oriented Concepts, Inc.
// Billerica, MA, USA
//
// All Rights Reserved
//
// **********************************************************************


package gnu.testlet.org.omg.PortableServer.POA;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import org.omg.CORBA.OBJECT_NOT_EXIST;
import org.omg.CORBA.ORB;
import org.omg.CORBA.SystemException;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAManager;
import org.omg.PortableServer.POAPackage.ObjectAlreadyActive;
import org.omg.PortableServer.POAPackage.ObjectNotActive;
import org.omg.PortableServer.POAPackage.ServantAlreadyActive;
import org.omg.PortableServer.POAPackage.ServantNotActive;
import org.omg.PortableServer.POAPackage.WrongPolicy;

public final class TestDeactivate
  extends TestBase
  implements Testlet
{
  static final class Test_impl2
    extends TestPOA
  {
    private POA poa_;
    private boolean called_ = false;
    private boolean finished_ = false;

    Test_impl2(POA poa)
    {
      poa_ = poa;
    }

    public synchronized void aMethod()
    {
      called_ = true;
      notify();

      try
        {
          wait(1000);
        }
      catch (InterruptedException ex)
        {
        }
      finished_ = true;
    }

    public POA _default_POA()
    {
      return poa_;
    }

    synchronized void blockUntilCalled()
    {
      while (!called_)
        {
          try
            {
              wait();
            }
          catch (InterruptedException ex)
            {
            }
        }
    }

    synchronized boolean callComplete()
    {
      return finished_;
    }
  }

  static final class LongCaller
    extends Thread
  {
    private Test t_;

    LongCaller(Test t)
    {
      t_ = t;
    }

    public void run()
    {
      try
        {
          t_.aMethod();
        }
      catch (SystemException ex)
        {
          ex.printStackTrace();
        }
    }
  }

  //
  // In this test we want to spawn a thread to call a method on the Test
  // interface. This method call should take some time. While the thread
  // is calling the method we attempt to deactivate the object. This
  // should not complete for some time, since it should wait for all
  // outstanding method calls to complete.
  //
  void mTestDeactivateThreaded(ORB orb, POA root)
  {
    Test_impl2 impl = new Test_impl2(root);

    Test t = impl._this(orb);

    byte[] oid = null;
    try
      {
        oid = root.servant_to_id(impl);
      }
    catch (ServantNotActive ex)
      {
        TEST(false);
      }
    catch (WrongPolicy ex)
      {
        TEST(false);
      }

    Thread thr = new LongCaller(t);

    thr.start();
    impl.blockUntilCalled();

    //
    // Test: deactivate_object while method call is active
    //
    try
      {
        root.deactivate_object(oid);
      }
    catch (ObjectNotActive ex)
      {
        TEST(false);
      }
    catch (WrongPolicy ex)
      {
        TEST(false);
      }

    //
    // Once we've deactivated the object the re-activation shouldn't
    // complete until the method call completes
    //
    try
      {
        root.activate_object_with_id(oid, impl);
      }
    catch (ObjectAlreadyActive ex)
      {
        TEST(false);
      }
    catch (ServantAlreadyActive ex)
      {
        TEST(false);
      }
    catch (WrongPolicy ex)
      {
        TEST(false);
      }

    //
    // Wait for the thread to terminate
    //
    while (thr.isAlive())
      {
        try
          {
            thr.join();
          }
        catch (InterruptedException ex)
          {
          }
      }

    try
      {
        root.deactivate_object(oid);
      }
    catch (ObjectNotActive ex)
      {
        TEST(false);
      }
    catch (WrongPolicy ex)
      {
        TEST(false);
      }
  }

  void mTestDeactivateBlocking(ORB orb, POA root)
  {
    Test_impl impl = new Test_impl(orb, "", false);

    Test t = impl._this(orb);

    t.aMethod();

    byte[] oid = null;
    try
      {
        oid = root.servant_to_id(impl);
      }
    catch (ServantNotActive ex)
      {
        TEST(false);
      }
    catch (WrongPolicy ex)
      {
        TEST(false);
      }

    try
      {
        root.deactivate_object(oid);
      }
    catch (ObjectNotActive ex)
      {
        TEST(false);
      }
    catch (WrongPolicy ex)
      {
        TEST(false);
      }

    try
      {
        t.aMethod();
        TEST(false); // expected OBJECT_NOT_EXIST
      }
    catch (OBJECT_NOT_EXIST ex)
      {
        // Expected
      }
  }

  public void testIt()
  {
    java.util.Properties props = System.getProperties();

    ORB orb = null;

    //
    // Create ORB
    //
    orb = ORB.init(new String[ 0 ], props);

    POA root = TestUtil.GetRootPOA(orb);

    //
    // Activate the RootPOA manager
    //
    POAManager rootMgr = root.the_POAManager();
    TEST(rootMgr != null);

    try
      {
        rootMgr.activate();
      }
    catch (org.omg.PortableServer.POAManagerPackage.AdapterInactive ex)
      {
        TEST(false);
      }

    //
    // Run the tests using the root POA
    //
    mTestDeactivateBlocking(orb, root);
    mTestDeactivateThreaded(orb, root);

    if (orb != null)
      orb.destroy();
  }

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    testIt();
  }
}