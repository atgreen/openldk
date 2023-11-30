// Tags: not-a-test
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
// Copyright (c) 2001
// Object Oriented Concepts, Inc.
// Billerica, MA, USA
//
// All Rights Reserved
//
// **********************************************************************


package gnu.testlet.org.omg.PortableServer.POA;

import org.omg.CORBA.ORB;
import org.omg.CORBA.Policy;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAManager;
import org.omg.PortableServer.POAPackage.AdapterAlreadyExists;
import org.omg.PortableServer.POAPackage.InvalidPolicy;
import org.omg.PortableServer.POAPackage.WrongPolicy;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;

final class TestLocationForwardServerMain
{
  static String ior = null;

  static final class Server_impl
    extends TestLocationForwardServerPOA
  {
    private ORB orb_;
    private TestLocationForwardActivator_impl activator_;
    private org.omg.CORBA.Object servant_;

    Server_impl(ORB orb, TestLocationForwardActivator_impl activator,
                org.omg.CORBA.Object servant
               )
    {
      orb_ = orb;
      activator_ = activator;
      servant_ = servant;
    }

    public void setForwardRequest(org.omg.CORBA.Object obj)
    {
      activator_.setForwardRequest(obj);
    }

    public org.omg.CORBA.Object get_servant()
    {
      return servant_;
    }

    public void deactivate()
    {
      orb_.shutdown(false);
    }
  }

  public static void main(String[] args)
  {
    java.util.Properties props = System.getProperties();

    ORB orb = null;

    //
    // Create ORB
    //
    orb = ORB.init(args, props);

    POA root = TestUtil.GetRootPOA(orb);

    POA poa;
    Policy[] policies;

    POAManager manager = root.the_POAManager();

    //
    // Create POAs
    //
    policies = new Policy[ 4 ];
    policies [ 0 ] =
      root.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);
    policies [ 1 ] =
      root.create_id_uniqueness_policy(org.omg.PortableServer.IdUniquenessPolicyValue.UNIQUE_ID);
    policies [ 2 ] =
      root.create_servant_retention_policy(org.omg.PortableServer.ServantRetentionPolicyValue.RETAIN);
    policies [ 3 ] =
      root.create_request_processing_policy(org.omg.PortableServer.RequestProcessingPolicyValue.USE_SERVANT_MANAGER);

    try
      {
        poa = root.create_POA("poa", manager, policies);
      }
    catch (AdapterAlreadyExists ex)
      {
        throw new RuntimeException(ex);
      }
    catch (InvalidPolicy ex)
      {
        throw new RuntimeException(ex);
      }

    TestLocationForwardActivator_impl activatorImpl =
      new TestLocationForwardActivator_impl();
    org.omg.PortableServer.ServantActivator activator =
      activatorImpl._this(orb);

    try
      {
        poa.set_servant_manager(activator);
      }
    catch (WrongPolicy ex)
      {
        throw new RuntimeException(ex);
      }

    byte[] oid = "test".getBytes();
    org.omg.CORBA.Object obj =
      poa.create_reference_with_id(oid, "IDL:Test:1.0");

    TestLocationForward_impl testImpl = new TestLocationForward_impl(orb);

    activatorImpl.setActivatedServant(testImpl);

    Server_impl serverImpl = new Server_impl(orb, activatorImpl, obj);
    TestLocationForwardServer server = serverImpl._this(orb);

    //
    // Save reference
    //
    String refFile = "Test.ref";
    try
      {
        FileOutputStream file = new FileOutputStream(refFile);
        PrintWriter out = new PrintWriter(file);
        out.println(ior = orb.object_to_string(server));
        out.flush();
        file.close();
      }
    catch (IOException ex)
      {
        throw new RuntimeException(ex);
      }

    //
    // Run implementation
    //
    try
      {
        manager.activate();
      }
    catch (org.omg.PortableServer.POAManagerPackage.AdapterInactive ex)
      {
        throw new RuntimeException(ex);
      }

    orb.run();

    File file = new File(refFile);
    file.delete();

    orb.destroy();
  }
}
