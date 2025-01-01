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

import gnu.testlet.*;

import org.omg.CORBA.ORB;
import org.omg.CORBA.Policy;
import org.omg.PortableServer.ForwardRequest;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAManager;
import org.omg.PortableServer.POAPackage.AdapterAlreadyExists;
import org.omg.PortableServer.POAPackage.InvalidPolicy;
import org.omg.PortableServer.POAPackage.ObjectAlreadyActive;
import org.omg.PortableServer.POAPackage.ObjectNotActive;
import org.omg.PortableServer.POAPackage.ServantAlreadyActive;
import org.omg.PortableServer.POAPackage.ServantNotActive;
import org.omg.PortableServer.POAPackage.WrongPolicy;
import org.omg.PortableServer.Servant;
import org.omg.PortableServer.ServantActivator;
import org.omg.PortableServer.ServantActivatorPOA;

/**
 * This test passes with Suns JDK 1.4.08_b_03 but fails with
 * Suns JDK 1.5.0._4 (regression). It should pass with
 * Gnu Classpath.
 */
public final class TestActivate
  extends TestBase
  implements Testlet
{
  public static final String regression_note =
    "This is known regression 1.4.08_b_03 -> 1.5.0._4";

  final class TestActivator_impl
    extends ServantActivatorPOA
  {
    private byte[] oid_;
    private POA poa_;
    private Servant servant_;
    private boolean valid_;

    void expect(byte[] oid, POA poa, Servant servant)
    {
      oid_ = oid;
      poa_ = poa;
      servant_ = servant;
      valid_ = false;
    }

    boolean isValid()
    {
      return valid_;
    }

    public Servant incarnate(byte[] oid, POA poa)
                      throws ForwardRequest
    {
      return null;
    }

    public void etherealize(byte[] oid, POA poa, Servant servant,
                            boolean cleanup, boolean remaining
                           )
    {
      TEST(TestUtil.Compare(oid_, oid));
      TEST(poa_._is_equivalent(poa));
      TEST(servant_ == servant);
      valid_ = true;
    }
  }

  private void run(ORB orb, POA root)
  {
    org.omg.CORBA.Object obj;
    POA system;
    POA nonretain;
    POA multiple;
    POA ether;
    byte[] id1;
    byte[] id2;
    byte[] id3;
    Policy[] policies;
    Test_impl servant1;
    Test_impl servant2;
    Servant tmpserv;

    POAManager manager = root.the_POAManager();

    try
      {
        manager.activate();
      }
    catch (org.omg.PortableServer.POAManagerPackage.AdapterInactive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Create POAs
    //
    policies = new Policy[ 3 ];
    policies [ 0 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.SYSTEM_ID);
    policies [ 1 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.UNIQUE_ID);
    policies [ 2 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    try
      {
        system = root.create_POA("system_id", manager, policies);
      }
    catch (AdapterAlreadyExists ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (InvalidPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    policies = new Policy[ 5 ];
    policies [ 0 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);
    policies [ 1 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.MULTIPLE_ID);
    policies [ 2 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.NON_RETAIN);
    policies [ 3 ] =
      root.create_request_processing_policy(org.omg.PortableServer.RequestProcessingPolicyValue.USE_DEFAULT_SERVANT);
    policies [ 4 ] =
      root.create_implicit_activation_policy(org.omg.PortableServer.ImplicitActivationPolicyValue.NO_IMPLICIT_ACTIVATION);
    try
      {
        nonretain = root.create_POA("nonretain", manager, policies);
      }
    catch (AdapterAlreadyExists ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (InvalidPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    policies = new Policy[ 3 ];
    policies [ 0 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.SYSTEM_ID);
    policies [ 1 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.MULTIPLE_ID);
    policies [ 2 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    try
      {
        multiple = root.create_POA("multiple_id", manager, policies);
      }
    catch (AdapterAlreadyExists ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (InvalidPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    policies = new Policy[ 3 ];
    policies [ 0 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.SYSTEM_ID);
    policies [ 1 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.MULTIPLE_ID);
    policies [ 2 ] =
      root.create_request_processing_policy(org.omg.PortableServer.RequestProcessingPolicyValue.USE_SERVANT_MANAGER);
    try
      {
        ether = root.create_POA("ether", manager, policies);
      }
    catch (AdapterAlreadyExists ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (InvalidPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    TestActivator_impl activatorImpl = new TestActivator_impl();
    ServantActivator activator = activatorImpl._this(orb);

    //
    // Start tests
    //
    try
      {
        ether.set_servant_manager(activator);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    servant1 = new Test_impl(orb, "obj1", false);
    servant2 = new Test_impl(orb, "obj2", false);

    //
    // Test: activate_object w/ SYSTEM_ID POA
    //
    try
      {
        id1 = system.activate_object(servant1);
        id2 = system.activate_object(servant2);
      }
    catch (ServantAlreadyActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(!TestUtil.Compare(id1, id2));
    try
      {
        tmpserv = system.id_to_servant(id1);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    TEST(tmpserv == servant1);
    try
      {
        tmpserv = system.id_to_servant(id2);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(tmpserv == servant2);

    //
    // Test: ServantAlreadyActive exception
    //
    try
      {
        system.activate_object(servant1);
        TEST(false); // activate_object should not have succeeded
      }
    catch (ServantAlreadyActive ex)
      {
        // expected
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    try
      {
        system.activate_object(servant2);
        TEST(false); // activate_object should not have succeeded
      }
    catch (ServantAlreadyActive ex)
      {
        // expected
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Test: deactivate_object
    //
    try
      {
        system.deactivate_object(id2);
        system.deactivate_object(id1);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Test: ObjectNotActive exception
    //
    try
      {
        system.deactivate_object(id1);
        TEST(false); // deactivate_object should not have succeeded
      }
    catch (ObjectNotActive ex)
      {
        // expected
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    try
      {
        system.deactivate_object(id2);
        TEST(false); // deactivate_object should not have succeeded
      }
    catch (ObjectNotActive ex)
      {
        // expected
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Test: WrongPolicy exception
    //
    try
      {
        nonretain.activate_object(servant1);
        TEST(false); // activate_object should not have succeeded
      }
    catch (WrongPolicy ex)
      {
        // expected
      }
    catch (ServantAlreadyActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    try
      {
        byte[] id = ("XXX").getBytes();
        nonretain.activate_object_with_id(id, servant1);
        TEST(false); // activate_object_with_id should not have succeeded
      }
    catch (WrongPolicy ex)
      {
        // expected
      }
    catch (ServantAlreadyActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ObjectAlreadyActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    try
      {
        byte[] id = ("XXX").getBytes();
        nonretain.deactivate_object(id);
        TEST(false); // deactivate_object should not have succeeded
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        // expected
      }

    //
    // Test: activate_object w/ MULTIPLE_ID POA
    //
    try
      {
        id1 = multiple.activate_object(servant1);
        id2 = multiple.activate_object(servant1);
      }
    catch (ServantAlreadyActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(!TestUtil.Compare(id1, id2));
    try
      {
        tmpserv = multiple.id_to_servant(id1);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(tmpserv == servant1);
    try
      {
        tmpserv = multiple.id_to_servant(id2);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(tmpserv == servant1);

    //
    // Test: confirm servant1 is no longer active
    //
    try
      {
        multiple.deactivate_object(id1);
        multiple.deactivate_object(id2);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    try
      {
        multiple.id_to_servant(id1);
      }
    catch (ObjectNotActive ex)
      {
        // expected
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    try
      {
        multiple.id_to_servant(id2);
      }
    catch (ObjectNotActive ex)
      {
        // expected
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Test: confirm ServantActivator::etherealize is invoked on
    // deactivate
    //
    try
      {
        id1 = ether.activate_object(servant1);
        id2 = ether.activate_object(servant1);
        id3 = ether.activate_object(servant2);
      }
    catch (ServantAlreadyActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    activatorImpl.expect(id1, ether, servant1);
    try
      {
        ether.deactivate_object(id1);
        Thread.sleep(2000);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (InterruptedException ex)
    {
      fail(ex);
      throw new RuntimeException(ex);
    }
    TEST(activatorImpl.isValid(), regression_note);
    activatorImpl.expect(id2, ether, servant1);
    try
      {
        ether.deactivate_object(id2);
        Thread.sleep(2000);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (InterruptedException ex)
    {
      fail(ex);
      throw new RuntimeException(ex);
    }
    TEST(activatorImpl.isValid(), regression_note);
    activatorImpl.expect(id3, ether, servant2);
    try
      {
        ether.deactivate_object(id3);
        Thread.sleep(2000);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (InterruptedException ex)
    {
      fail(ex);
      throw new RuntimeException(ex);
    }
    TEST(activatorImpl.isValid(), "Regression in 1.5");

    system.destroy(true, true);
    nonretain.destroy(true, true);
    multiple.destroy(true, true);
    ether.destroy(true, true);

    //
    // Since activatorImpl is a stack-based servant, we need to deactivate
    // it before it goes out of scope
    //
    byte[] id = null;
    try
      {
        id = root.servant_to_id(activatorImpl);
      }
    catch (ServantNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    try
      {
        root.deactivate_object(id);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    tmpserv = null;
    servant1 = null;
    servant2 = null;
  }

  public void testIt()
  {
    java.util.Properties props = System.getProperties();

    ORB orb = null;

    orb = ORB.init(new String[ 0 ], props);

    POA root = TestUtil.GetRootPOA(orb);

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