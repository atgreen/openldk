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
import org.omg.PortableServer.POAPackage.AdapterAlreadyExists;
import org.omg.PortableServer.POAPackage.AdapterNonExistent;
import org.omg.PortableServer.POAPackage.InvalidPolicy;

public final class TestFind
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
    // Create child POA
    //
    try
      {
        poa = root.create_POA("poa1", rootMgr, policies);
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
    // Test: find_POA
    //
    try
      {
        poa2 = root.find_POA("poa1", false);
      }
    catch (AdapterNonExistent ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(poa2 != null);
    TEST(poa2._is_equivalent(poa));

    //
    // Test: AdapterNonExistent exception
    //
    try
      {
        poa2 = root.find_POA("poaX", false);
        TEST(false); // find_POA should not have succeeded
      }
    catch (AdapterNonExistent ex)
      {
        // expected
      }

    //
    // Create child POA
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
    // Test: Confirm parent knows about child
    //
    try
      {
        poa3 = root.find_POA("poa2", false);
      }
    catch (AdapterNonExistent ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    TEST(poa3 != null);
    TEST(poa3._is_equivalent(poa2));
  }

  public void testIt()
  {
    java.util.Properties props = System.getProperties();

    int status = 0;
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
      orb.destroy();
  }

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    testIt();
  }
}