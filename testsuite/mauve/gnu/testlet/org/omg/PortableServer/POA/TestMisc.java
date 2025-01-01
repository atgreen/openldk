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

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Policy;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAManager;
import org.omg.PortableServer.POAPackage.AdapterAlreadyExists;
import org.omg.PortableServer.POAPackage.InvalidPolicy;
import org.omg.PortableServer.POAPackage.ObjectAlreadyActive;
import org.omg.PortableServer.POAPackage.ObjectNotActive;
import org.omg.PortableServer.POAPackage.ServantAlreadyActive;
import org.omg.PortableServer.POAPackage.ServantNotActive;
import org.omg.PortableServer.POAPackage.WrongAdapter;
import org.omg.PortableServer.POAPackage.WrongPolicy;
import org.omg.PortableServer.Servant;

public final class TestMisc
  extends TestBase
  implements Testlet
{
  void uTestCreateReference(ORB orb, POA root)
  {
    org.omg.CORBA.Object obj;
    POA user;
    POA system;
    byte[] id1;
    byte[] id2;
    byte[] tmpid;
    Policy[] policies;

    POAManager manager = root.the_POAManager();

    policies = new Policy[ 1 ];
    policies [ 0 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);
    try
      {
        user = root.create_POA("user_id", manager, policies);
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

    policies = new Policy[ 1 ];
    policies [ 0 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.SYSTEM_ID);
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

    //
    // Test: create_reference with wrong POA policies
    //
    try
      {
        obj = user.create_reference("IDL:Test:1.0");
        TEST(false); // create_reference should not have succeeded
      }
    catch (WrongPolicy ex)
      {
        // expected
      }

    //
    // Test: create_reference - should get a new ID for each invocation
    //       on POA w/ SYSTEM_ID policy
    //
    try
      {
        obj = system.create_reference("IDL:Test:1.0");
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(obj != null);
    try
      {
        id1 = system.reference_to_id(obj);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongAdapter ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    try
      {
        obj = system.create_reference("IDL:Test:1.0");
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(obj != null);
    try
      {
        id2 = system.reference_to_id(obj);
      }
    catch (WrongAdapter ex)
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

    //
    // Test: create_reference_with_id using a system-generated ID
    //
    try
      {
        obj = system.create_reference_with_id(id1, "IDL:Test:1.0");
      }
    catch (BAD_PARAM ex)
      {
        TEST(false); // create_reference_with_id should have succeeded
      }

    id1 = ("id1").getBytes();

    //
    // Test: create_reference_with_id
    //
    obj = user.create_reference_with_id(id1, "IDL:Test:1.0");
    TEST(obj != null);
    try
      {
        tmpid = user.reference_to_id(obj);
      }
    catch (WrongAdapter ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(TestUtil.Compare(id1, tmpid));
    id2 = ("id2").getBytes();
    obj = user.create_reference_with_id(id2, "IDL:Test:1.0");
    TEST(obj != null);
    try
      {
        tmpid = user.reference_to_id(obj);
      }
    catch (WrongAdapter ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(TestUtil.Compare(id2, tmpid));

    user.destroy(true, true);
    system.destroy(true, true);
  }

  void uTestServantToId(ORB orb, POA root)
  {
    org.omg.CORBA.Object obj;
    POA unique;
    POA implicit;
    POA multiple;
    byte[] id1;
    byte[] id2;
    byte[] tmpid;
    Policy[] policies;
    Test_impl servant1;
    Test_impl servant2;

    POAManager manager = root.the_POAManager();

    //
    // Create POA w/ UNIQUE_ID, NO_IMPLICIT_ACTIVATION
    //
    policies = new Policy[ 4 ];
    policies [ 0 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.UNIQUE_ID);
    policies [ 1 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);
    policies [ 2 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 3 ] =
      root.create_implicit_activation_policy(org.omg.PortableServer.ImplicitActivationPolicyValue.NO_IMPLICIT_ACTIVATION);
    try
      {
        unique = root.create_POA("unique_id", manager, policies);
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

    //
    // Create POA w/ UNIQUE_ID, IMPLICIT_ACTIVATION
    //
    policies = new Policy[ 3 ];
    policies [ 0 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.UNIQUE_ID);
    policies [ 1 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 2 ] =
      root.create_implicit_activation_policy(org.omg.PortableServer.ImplicitActivationPolicyValue.IMPLICIT_ACTIVATION);
    try
      {
        implicit = root.create_POA("implicit", manager, policies);
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

    //
    // Create POA w/ MULTIPLE_ID, IMPLICIT_ACTIVATION
    //
    policies = new Policy[ 3 ];
    policies [ 0 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.MULTIPLE_ID);
    policies [ 1 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 2 ] =
      root.create_implicit_activation_policy(org.omg.PortableServer.ImplicitActivationPolicyValue.IMPLICIT_ACTIVATION);
    try
      {
        multiple = root.create_POA("multiple", manager, policies);
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

    servant1 = new Test_impl(orb, "test1", false);
    servant2 = new Test_impl(orb, "test2", false);

    //
    // Test: ServantNotActive exception
    //
    try
      {
        unique.servant_to_id(servant1);
        TEST(false); // servant_to_id should not have succeeded
      }
    catch (ServantNotActive ex)
      {
        // expected
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    id1 = ("test1").getBytes();
    try
      {
        unique.activate_object_with_id(id1, servant1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
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

    //
    // Test: servant_to_id (UNIQUE_ID policy)
    //
    try
      {
        tmpid = unique.servant_to_id(servant1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(TestUtil.Compare(id1, tmpid));

    //
    // Test: servant_to_id (IMPLICIT_ACTIVATION) - servant1 should
    //       be automatically activated
    //
    try
      {
        id1 = implicit.servant_to_id(servant1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Test: Now that servant1 is activated, and since we have UNIQUE_ID,
    //       we should get the same ID back
    //
    try
      {
        tmpid = implicit.servant_to_id(servant1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(TestUtil.Compare(id1, tmpid));

    //
    // Test: Implicitly activating servant2 should produce a new ID
    //
    try
      {
        id2 = implicit.servant_to_id(servant2);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(!TestUtil.Compare(id1, id2));

    //
    // Test: servant_to_id (IMPLICIT_ACTIVATION, MULTIPLE_ID) - servant1
    //       should be automatically activated
    //
    try
      {
        id1 = multiple.servant_to_id(servant1);
      }
    catch (WrongPolicy ex)
      {
        ex.printStackTrace();
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Test: Since we have MULTIPLE_ID, we should get a new ID
    //
    try
      {
        tmpid = multiple.servant_to_id(servant1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(!TestUtil.Compare(id1, tmpid));

    unique.destroy(true, true);
    implicit.destroy(true, true);
    multiple.destroy(true, true);
  }

  void uTestIdToServant(ORB orb, POA root)
  {
    org.omg.CORBA.Object obj;
    POA retain;
    POA defaultPOA;
    byte[] id1;
    byte[] id2;
    byte[] tmpid;
    Policy[] policies;
    Test_impl def;
    Test_impl servant1;
    Test_impl servant2;
    Servant tmpservant;

    POAManager manager = root.the_POAManager();

    //
    // Create POA w/ RETAIN
    //
    policies = new Policy[ 3 ];
    policies [ 0 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 1 ] =
      root.create_lifespan_policy(org.omg.PortableServer.LifespanPolicyValue.TRANSIENT);
    policies [ 2 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);
    try
      {
        retain = root.create_POA("retain", manager, policies);
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

    //
    // Create POA w/ USE_DEFAULT_SERVANT
    //
    policies = new Policy[ 5 ];
    policies [ 0 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.MULTIPLE_ID);
    policies [ 1 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 2 ] =
      root.create_request_processing_policy(org.omg.PortableServer.RequestProcessingPolicyValue.USE_DEFAULT_SERVANT);
    policies [ 3 ] =
      root.create_lifespan_policy(org.omg.PortableServer.LifespanPolicyValue.TRANSIENT);
    policies [ 4 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);

    try
      {
        defaultPOA = root.create_POA("default", manager, policies);
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
    def = new Test_impl(orb, "default", false);
    try
      {
        defaultPOA.set_servant(def);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    servant1 = new Test_impl(orb, "test1", false);
    servant2 = new Test_impl(orb, "test2", false);

    //
    // Test: ObjectNotActive exception
    //
    try
      {
        tmpid = ("bad_id").getBytes();
        retain.id_to_servant(tmpid);
        TEST(false); // id_to_servant should not have succeeded
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

    id1 = ("test1").getBytes();
    id2 = ("test2").getBytes();
    try
      {
        retain.activate_object_with_id(id1, servant1);
        retain.activate_object_with_id(id2, servant2);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantAlreadyActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ObjectAlreadyActive ex)
      {
        ex.printStackTrace();
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Test: servant_to_id (RETAIN policy)
    //
    try
      {
        tmpservant = retain.id_to_servant(id1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(servant1 == tmpservant);
    try
      {
        tmpservant = retain.id_to_servant(id2);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(servant2 == tmpservant);

    //
    // Test: id_to_servant (USE_DEFAULT_SERVANT)
    //
    try
      {
        defaultPOA.activate_object_with_id(id1, servant1);
        defaultPOA.activate_object_with_id(id2, servant2);
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
    catch (ObjectAlreadyActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    try
      {
        tmpservant = defaultPOA.id_to_servant(id1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(servant1 == tmpservant);
    try
      {
        tmpservant = defaultPOA.id_to_servant(id2);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(servant2 == tmpservant);

    retain.destroy(true, true);
    defaultPOA.destroy(true, true);
  }

  void uTestServantToReference(ORB orb, POA root)
  {
    org.omg.CORBA.Object obj;
    POA unique;
    POA implicit;
    POA multiple;
    byte[] id1;
    byte[] id2;
    byte[] tmpid1;
    byte[] tmpid2;
    Policy[] policies;
    Test_impl servant1;
    Test_impl servant2;

    POAManager manager = root.the_POAManager();

    //
    // Create POA w/ UNIQUE_ID, NO_IMPLICIT_ACTIVATION
    //
    policies = new Policy[ 4 ];
    policies [ 0 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.UNIQUE_ID);
    policies [ 1 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);
    policies [ 2 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 3 ] =
      root.create_implicit_activation_policy(org.omg.PortableServer.ImplicitActivationPolicyValue.NO_IMPLICIT_ACTIVATION);
    try
      {
        unique = root.create_POA("unique_id", manager, policies);
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

    //
    // Create POA w/ UNIQUE_ID, IMPLICIT_ACTIVATION
    //
    policies = new Policy[ 3 ];
    policies [ 0 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.UNIQUE_ID);
    policies [ 1 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 2 ] =
      root.create_implicit_activation_policy(org.omg.PortableServer.ImplicitActivationPolicyValue.IMPLICIT_ACTIVATION);
    try
      {
        implicit = root.create_POA("implicit", manager, policies);
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

    //
    // Create POA w/ MULTIPLE_ID, IMPLICIT_ACTIVATION
    //
    policies = new Policy[ 3 ];
    policies [ 0 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.MULTIPLE_ID);
    policies [ 1 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 2 ] =
      root.create_implicit_activation_policy(org.omg.PortableServer.ImplicitActivationPolicyValue.IMPLICIT_ACTIVATION);
    try
      {
        multiple = root.create_POA("multiple", manager, policies);
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

    servant1 = new Test_impl(orb, "test1", false);
    servant2 = new Test_impl(orb, "test2", false);

    //
    // Test: ServantNotActive exception
    //
    try
      {
        unique.servant_to_reference(servant1);
        TEST(false); // servant_to_reference should not have succeeded
      }
    catch (ServantNotActive ex)
      {
        // expected
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    id1 = ("test1").getBytes();
    try
      {
        unique.activate_object_with_id(id1, servant1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
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

    //
    // Test: servant_to_reference (UNIQUE_ID policy)
    //
    try
      {
        obj = unique.servant_to_reference(servant1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(obj != null);
    try
      {
        tmpid1 = unique.reference_to_id(obj);
      }
    catch (WrongAdapter ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(TestUtil.Compare(id1, tmpid1));

    //
    // Test: servant_to_reference (IMPLICIT_ACTIVATION) - servant1 should
    //       be automatically activated
    //
    try
      {
        obj = implicit.servant_to_reference(servant1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(obj != null);
    try
      {
        tmpid1 = implicit.reference_to_id(obj);
      }
    catch (WrongAdapter ex)
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
    // Test: Now that servant1 is activated, and since we have UNIQUE_ID,
    //       we should get the same ID back
    //
    try
      {
        obj = implicit.servant_to_reference(servant1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(obj != null);
    try
      {
        tmpid2 = implicit.reference_to_id(obj);
      }
    catch (WrongAdapter ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(TestUtil.Compare(tmpid1, tmpid2));

    //
    // Test: Implicitly activating servant2 should produce a new ID
    //
    try
      {
        obj = implicit.servant_to_reference(servant2);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(obj != null);
    try
      {
        tmpid2 = implicit.reference_to_id(obj);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongAdapter ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(!TestUtil.Compare(tmpid1, tmpid2));

    //
    // Test: servant_to_reference (IMPLICIT_ACTIVATION, MULTIPLE_ID) -
    //       servant1 should be automatically activated
    //
    try
      {
        obj = multiple.servant_to_reference(servant1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(obj != null);
    try
      {
        tmpid1 = multiple.reference_to_id(obj);
      }
    catch (WrongAdapter ex)
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
    // Test: Since we have MULTIPLE_ID, we should get a new ID
    //
    try
      {
        obj = multiple.servant_to_reference(servant1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(obj != null);
    try
      {
        tmpid2 = multiple.reference_to_id(obj);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongAdapter ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(!TestUtil.Compare(tmpid1, tmpid2));

    unique.destroy(true, true);
    implicit.destroy(true, true);
    multiple.destroy(true, true);
  }

  void uTestIdToReference(ORB orb, POA root)
  {
    org.omg.CORBA.Object obj;
    POA retain;
    POA defaultPOA;
    byte[] id1;
    byte[] id2;
    byte[] tmpid;
    Policy[] policies;
    Test_impl servant1;
    Test_impl servant2;

    POAManager manager = root.the_POAManager();

    //
    // Create POA w/ RETAIN
    //
    policies = new Policy[ 3 ];
    policies [ 0 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 1 ] =
      root.create_lifespan_policy(org.omg.PortableServer.LifespanPolicyValue.TRANSIENT);
    policies [ 2 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);
    try
      {
        retain = root.create_POA("retain", manager, policies);
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

    servant1 = new Test_impl(orb, "test1", false);
    servant2 = new Test_impl(orb, "test2", false);

    //
    // Test: ObjectNotActive exception
    //
    try
      {
        tmpid = ("bad_id").getBytes();
        retain.id_to_reference(tmpid);
        TEST(false); // id_to_reference should not have succeeded
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

    id1 = ("test1").getBytes();
    id2 = ("test2").getBytes();
    try
      {
        retain.activate_object_with_id(id1, servant1);
        retain.activate_object_with_id(id2, servant2);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
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

    //
    // Test: servant_to_reference
    //
    try
      {
        obj = retain.id_to_reference(id1);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(obj != null);
    try
      {
        tmpid = retain.reference_to_id(obj);
      }
    catch (WrongAdapter ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(TestUtil.Compare(id1, tmpid));

    //
    // Test: servant_to_reference
    //
    try
      {
        obj = retain.id_to_reference(id2);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(obj != null);
    try
      {
        tmpid = retain.reference_to_id(obj);
      }
    catch (WrongAdapter ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(TestUtil.Compare(id2, tmpid));

    retain.destroy(true, true);
  }

  void uTestReferenceToServant(ORB orb, POA root)
  {
    org.omg.CORBA.Object obj;
    POA retain;
    POA defaultPOA;
    byte[] id1;
    byte[] id2;
    byte[] tmpid;
    Policy[] policies;
    Test_impl def;
    Test_impl servant1;
    Test_impl servant2;
    Servant tmpservant;

    POAManager manager = root.the_POAManager();

    //
    // Create POA w/ RETAIN
    //
    policies = new Policy[ 3 ];
    policies [ 0 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 1 ] =
      root.create_lifespan_policy(org.omg.PortableServer.LifespanPolicyValue.TRANSIENT);
    policies [ 2 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);
    try
      {
        retain = root.create_POA("retain", manager, policies);
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

    //
    // Create POA w/ USE_DEFAULT_SERVANT
    //
    policies = new Policy[ 5 ];
    policies [ 0 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.MULTIPLE_ID);
    policies [ 1 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 2 ] =
      root.create_request_processing_policy(org.omg.PortableServer.RequestProcessingPolicyValue.USE_DEFAULT_SERVANT);
    policies [ 3 ] =
      root.create_lifespan_policy(org.omg.PortableServer.LifespanPolicyValue.TRANSIENT);
    policies [ 4 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);

    try
      {
        defaultPOA = root.create_POA("default", manager, policies);
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
    def = new Test_impl(orb, "default", false);
    try
      {
        defaultPOA.set_servant(def);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    servant1 = new Test_impl(orb, "test1", false);
    servant2 = new Test_impl(orb, "test2", false);

    //
    // Test: ObjectNotActive exception
    //
    try
      {
        tmpid = ("bad_id").getBytes();
        obj = retain.create_reference_with_id(tmpid, "IDL:Test:1.0");
        retain.reference_to_servant(obj);
        TEST(false); // reference_to_servant should not have succeeded
      }
    catch (ObjectNotActive ex)
      {
        // expected
      }
    catch (WrongAdapter ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    id1 = ("test1").getBytes();
    id2 = ("test2").getBytes();
    try
      {
        retain.activate_object_with_id(id1, servant1);
        retain.activate_object_with_id(id2, servant2);
      }
    catch (ObjectAlreadyActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantAlreadyActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Test: reference_to_servant (USE_DEFAULT_SERVANT)
    //
    try
      {
        defaultPOA.activate_object_with_id(id1, servant1);
        defaultPOA.activate_object_with_id(id2, servant2);
      }
    catch (ObjectAlreadyActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ServantAlreadyActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    //
    // Test: reference_to_servant (USE_DEFAULT_SERVANT) - should return
    //       default servant for all unknown IDs
    //
    tmpid = ("test99").getBytes();
    obj = defaultPOA.create_reference_with_id(tmpid, "IDL:Test:1.0");
    try
      {
        tmpservant = defaultPOA.reference_to_servant(obj);
      }
    catch (WrongAdapter ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (ObjectNotActive ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(tmpservant == def);
    tmpservant = null;

    retain.destroy(true, true);
    defaultPOA.destroy(true, true);
  }

  void uTestReferenceToId(ORB orb, POA root)
  {
    org.omg.CORBA.Object obj;
    POA poa;
    byte[] id1;
    byte[] id2;
    byte[] tmpid;
    Policy[] policies;

    POAManager manager = root.the_POAManager();

    //
    // Create POA
    //
    policies = new Policy[ 3 ];
    policies [ 0 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 1 ] =
      root.create_lifespan_policy(org.omg.PortableServer.LifespanPolicyValue.TRANSIENT);
    policies [ 2 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);
    try
      {
        poa = root.create_POA("poa", manager, policies);
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

    id1 = ("test1").getBytes();
    id2 = ("test2").getBytes();

    //
    // Test: reference_to_id
    //
    obj = poa.create_reference_with_id(id1, "IDL:Test:1.0");
    try
      {
        tmpid = poa.reference_to_id(obj);
      }
    catch (WrongAdapter ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(TestUtil.Compare(tmpid, id1));
    obj = poa.create_reference_with_id(id2, "IDL:Test:1.0");
    try
      {
        tmpid = poa.reference_to_id(obj);
      }
    catch (WrongAdapter ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    TEST(TestUtil.Compare(tmpid, id2));

    //
    // Test: WrongAdapter exception
    //
    try
      {
        obj = poa.create_reference_with_id(id1, "IDL:Test:1.0");
        root.reference_to_id(obj);
        TEST(false); // reference_to_id should not have succeeded
      }
    catch (WrongAdapter ex)
      {
        // expected
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }

    poa.destroy(true, true);
  }

  void runtests(ORB orb, POA root)
  {
    uTestCreateReference(orb, root);
    uTestServantToId(orb, root);
    uTestIdToServant(orb, root);
    uTestServantToReference(orb, root);
    uTestIdToReference(orb, root);
    uTestReferenceToServant(orb, root);
    uTestReferenceToId(orb, root);
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
    // Run the tests using the root POA
    //
    runtests(orb, root);

    //
    // Create a child POA and run the tests again using the
    // child as the root
    //
    Policy[] policies = new Policy[ 0 ];
    POAManager manager = root.the_POAManager();
    POA child = null;
    try
      {
        child = root.create_POA("child", manager, policies);
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
    runtests(orb, child);

    if (orb != null)
      orb.destroy();
  }

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    testIt();
  }
}