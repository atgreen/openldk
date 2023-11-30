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

import org.omg.CORBA.ORB;
import org.omg.CORBA.Policy;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAManager;
import org.omg.PortableServer.POAManagerPackage.State;
import org.omg.PortableServer.POAPackage.AdapterAlreadyExists;
import org.omg.PortableServer.POAPackage.InvalidPolicy;

public final class TestCreate
  extends TestBase
  implements Testlet
{
  void run(ORB orb, POA root)
  {
    org.omg.CORBA.Object obj;
    Policy[] policies = new Policy[ 0 ];
    POA poa;
    POA parent;
    POA poa2;
    POA poa3;
    POAManager mgr;
    String str;

    POAManager rootMgr = root.the_POAManager();
    TEST(rootMgr != null);

    //
    // Test: POAManager should be in HOLDING state
    //
    TEST(rootMgr.get_state() == State.HOLDING);

    //
    // Create child POA
    //
    try
      {
        poa = root.create_POA("poa1", null, policies);
      }
    catch (InvalidPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (AdapterAlreadyExists ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Test: POAManager should NOT be the same as the root's manager
    //
    mgr = poa.the_POAManager();
    TEST(!mgr._is_equivalent(rootMgr));

    //
    // Test: POAManager should be in HOLDING state
    //
    TEST(mgr.get_state() == State.HOLDING);

    //
    // Test: Confirm name
    //
    str = poa.the_name();
    TEST(str.equals("poa1"));

    //
    // Test: Confirm parent
    //
    parent = poa.the_parent();
    TEST(parent._is_equivalent(root));

    //
    // Test: AdapterAlreadyExists exception
    //
    try
      {
        poa2 = root.create_POA("poa1", null, policies);
        TEST(false); // create_POA should not have succeeded
      }
    catch (AdapterAlreadyExists ex)
      {
        // expected
      }
    catch (InvalidPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Test: InvalidPolicy exception
    //
    Policy[] invalidpolicies = new Policy[ 1 ];
    invalidpolicies [ 0 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.NON_RETAIN);

    //
    // Create another child of root POA
    //
    try
      {
        poa2 = root.create_POA("poa2", rootMgr, policies);
      }
    catch (InvalidPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (AdapterAlreadyExists ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Test: POAManager should be the same as the root's manager
    //
    mgr = poa2.the_POAManager();
    TEST(mgr._is_equivalent(rootMgr));

    //
    // Create child of child POA
    //
    try
      {
        poa3 = poa2.create_POA("child", rootMgr, policies);
      }
    catch (InvalidPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (AdapterAlreadyExists ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Test: Confirm parent
    //
    parent = poa3.the_parent();
    TEST(parent._is_equivalent(poa2));

    poa.destroy(true, true);
    poa2.destroy(true, true);
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
    // Run the test
    //
    run(orb, root);

    if (orb != null)
      {
        orb.destroy();
      }
  }

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    testIt();
  }
}