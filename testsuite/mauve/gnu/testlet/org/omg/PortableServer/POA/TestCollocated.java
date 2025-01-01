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
import org.omg.CORBA.Request;
import org.omg.PortableServer.ForwardRequest;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAManager;
import org.omg.PortableServer.POAPackage.AdapterAlreadyExists;
import org.omg.PortableServer.POAPackage.InvalidPolicy;
import org.omg.PortableServer.POAPackage.ObjectNotActive;
import org.omg.PortableServer.POAPackage.ServantNotActive;
import org.omg.PortableServer.POAPackage.WrongPolicy;
import org.omg.PortableServer.Servant;
import org.omg.PortableServer.ServantActivator;
import org.omg.PortableServer.ServantActivatorPOA;
import org.omg.PortableServer.ServantLocator;
import org.omg.PortableServer.ServantLocatorPOA;
import org.omg.PortableServer.ServantLocatorPackage.CookieHolder;

public final class TestCollocated
  extends TestBase
  implements Testlet
{
  static final class TestLocator_impl
    extends ServantLocatorPOA
  {
    private ORB orb_;
    private Test_impl test_;
    private TestDSIRef_impl testDSI_;

    TestLocator_impl(ORB orb)
    {
      orb_ = orb;

      test_ = new Test_impl(orb, "locator_SSI", false);
      testDSI_ = new TestDSIRef_impl(orb, "locator_DSI", false);
    }

    public Servant preinvoke(byte[] oid, POA poa, String operation,
                             CookieHolder the_cookie
                            )
                      throws ForwardRequest
    {
      String oidString = new String(oid);

      if (oidString.equals("test"))
        return test_;
      else if (oidString.equals("testDSI"))
        return testDSI_;
      return null;
    }

    public void postinvoke(byte[] oid, POA poa, String operation,
                           java.lang.Object the_cookie, Servant the_servant
                          )
    {
    }
  }

  static final class TestActivator_impl
    extends ServantActivatorPOA
  {
    private ORB orb_;
    private Test_impl test_;
    private TestDSIRef_impl testDSI_;

    TestActivator_impl(ORB orb)
    {
      orb_ = orb;

      test_ = new Test_impl(orb, "locator_SSI", false);
      testDSI_ = new TestDSIRef_impl(orb, "locator_DSI", false);
    }

    public Servant incarnate(byte[] oid, POA poa)
                      throws ForwardRequest
    {
      String oidString = new String(oid);

      if (oidString.equals("test"))
        return test_;
      else if (oidString.equals("testDSI"))
        return testDSI_;

      //
      // Fail
      //
      return null;
    }

    public void etherealize(byte[] oid, POA poa, Servant servant,
                            boolean cleanup, boolean remaining
                           )
    {
      String oidString = new String(oid);

      if (!remaining)
        {
          if (oidString.equals("test"))
            {
              servant = null;
              test_ = null;
            }
          else if (oidString.equals("testDSI"))
            {
              testDSI_ = null;
            }
        }
    }
  }

  void uTestPOA(POA poa)
  {
    byte[] id;
    org.omg.CORBA.Object obj;
    Request request;
    Test test;

    //
    // Invoke twice on each object - statically & DII
    //
    id = ("test").getBytes();
    obj = poa.create_reference_with_id(id, "IDL:test/poa/Test:1.0");
    test = TestHelper.narrow(obj);
    test.aMethod();
    request = obj._request("aMethod");
    request.invoke();
    TEST(request.env().exception() == null);

    id = ("testDSI").getBytes();
    obj = poa.create_reference_with_id(id, "IDL:test/poa/Test:1.0");
    test = TestHelper.narrow(obj);
    test.aMethod();
    request = obj._request("aMethod");
    request.invoke();
    TEST(request.env().exception() == null);
  }

  void uTestDefaultServant(ORB orb, POA root, POAManager manager)
  {
    POA poa;
    Servant servant;
    Policy[] policies;

    //
    // Setup policies for default servant
    //
    policies = new Policy[ 6 ];
    policies [ 0 ] =
      root.create_lifespan_policy(org.omg.PortableServer.LifespanPolicyValue.TRANSIENT);
    policies [ 1 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);
    policies [ 2 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.NON_RETAIN);
    policies [ 3 ] =
      root.create_implicit_activation_policy(org.omg.PortableServer.ImplicitActivationPolicyValue.NO_IMPLICIT_ACTIVATION);
    policies [ 4 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.MULTIPLE_ID);
    policies [ 5 ] =
      root.create_request_processing_policy(org.omg.PortableServer.RequestProcessingPolicyValue.USE_DEFAULT_SERVANT);

    //
    // Create POA w/ static Default Servant
    //
    try
      {
        poa = root.create_POA("defaultSSI", manager, policies);
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

    Test_impl staticServant = new Test_impl(orb, "defaultStaticServant", false);
    try
      {
        poa.set_servant(staticServant);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    uTestPOA(poa);
    poa.destroy(true, true);

    //
    // Since staticServant is a stack-based servant, we need to deactivate
    // it before it goes out of scope
    //
    byte[] id = null;
    try
      {
        id = root.servant_to_id(staticServant);
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

    //
    // Create POA w/ DSI Default Servant
    //
    try
      {
        poa = root.create_POA("defaultDSI", manager, policies);
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
    servant = new TestDSIRef_impl(orb, "defaultDSIServant", false);
    try
      {
        poa.set_servant(servant);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    uTestPOA(poa);
    poa.destroy(true, true);
    servant = null;

    //
    // Clean up policies
    //
    for (int i = 0; i < policies.length; i++)
      policies [ i ].destroy();
  }

  void uTestServantLocator(ORB orb, POA root, POAManager manager)
  {
    POA poa;
    Servant servant;
    Policy[] policies;

    //
    // Setup policies for servant locator
    //
    policies = new Policy[ 6 ];
    policies [ 0 ] =
      root.create_lifespan_policy(org.omg.PortableServer.LifespanPolicyValue.TRANSIENT);
    policies [ 1 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);
    policies [ 2 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.NON_RETAIN);
    policies [ 3 ] =
      root.create_implicit_activation_policy(org.omg.PortableServer.ImplicitActivationPolicyValue.NO_IMPLICIT_ACTIVATION);
    policies [ 4 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.UNIQUE_ID);
    policies [ 5 ] =
      root.create_request_processing_policy(org.omg.PortableServer.RequestProcessingPolicyValue.USE_SERVANT_MANAGER);

    //
    // Create POA w/ Servant Locator
    //
    try
      {
        poa = root.create_POA("servloc", manager, policies);
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

    TestLocator_impl locatorImpl = new TestLocator_impl(orb);
    ServantLocator locator = locatorImpl._this(orb);
    try
      {
        poa.set_servant_manager(locator);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    uTestPOA(poa);
    poa.destroy(true, true);

    //
    // Clean up policies
    //
    for (int i = 0; i < policies.length; i++)
      policies [ i ].destroy();

    //
    // Since locatorImpl is a stack-based servant, we need to deactivate
    // it before it goes out of scope
    //
    byte[] id = null;
    try
      {
        id = root.servant_to_id(locatorImpl);
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
  }

  void uTestServantActivator(ORB orb, POA root, POAManager manager)
  {
    POA poa;
    Servant servant;
    Policy[] policies;

    //
    // Setup policies for servant activator
    //
    policies = new Policy[ 4 ];
    policies [ 0 ] =
      root.create_lifespan_policy(org.omg.PortableServer.LifespanPolicyValue.TRANSIENT);
    policies [ 1 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 2 ] =
      root.create_implicit_activation_policy(org.omg.PortableServer.ImplicitActivationPolicyValue.IMPLICIT_ACTIVATION);
    policies [ 3 ] =
      root.create_request_processing_policy(org.omg.PortableServer.RequestProcessingPolicyValue.USE_SERVANT_MANAGER);

    //
    // Create POA w/ Servant Activator
    //
    try
      {
        poa = root.create_POA("servant", manager, policies);
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

    TestActivator_impl activatorImpl = new TestActivator_impl(orb);
    ServantActivator activator = activatorImpl._this(orb);
    try
      {
        poa.set_servant_manager(activator);
      }
    catch (WrongPolicy ex)
      {
        fail(ex);
        throw new RuntimeException(ex);
      }
    uTestPOA(poa);
    poa.destroy(true, true);

    //
    // Clean up policies
    //
    for (int i = 0; i < policies.length; i++)
      policies [ i ].destroy();

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
  }

  public void testIt()
  {
    java.util.Properties props = System.getProperties();

    ORB orb = ORB.init(new String[ 0 ], props);

    POA root = TestUtil.GetRootPOA(orb);
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

    uTestDefaultServant(orb, root, manager);
    uTestServantLocator(orb, root, manager);
    uTestServantActivator(orb, root, manager);

    orb.destroy();
  }

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    testIt();
  }
}