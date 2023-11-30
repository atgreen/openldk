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
// Copyright (c) 2000
// Object Oriented Concepts, Inc.
// Billerica, MA, USA
//
// All Rights Reserved
//
// **********************************************************************


package gnu.testlet.org.omg.PortableServer.POA;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.NVList;
import org.omg.CORBA.ORB;
import org.omg.CORBA.ServerRequest;
import org.omg.CORBA.UserException;
import org.omg.PortableServer.DynamicImplementation;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAPackage.ServantNotActive;
import org.omg.PortableServer.POAPackage.WrongPolicy;

final class TestDSIRef_impl
  extends org.omg.PortableServer.DynamicImplementation
{
  private ORB orb_;
  private String name_;
  private boolean compare_;
  private boolean defaultServant_;

  //
  // From TestBase (no multiple inheritance)
  //
  public static void TEST(boolean expr)
  {
    if (!expr)
      throw new TestException();
  }

  TestDSIRef_impl(ORB orb, String name, boolean compare)
  {
    orb_ = orb;
    name_ = name;
    compare_ = compare;
  }

  void setDefaultServant(boolean b)
  {
    defaultServant_ = b;
  }

  //
  // Standard IDL to Java Mapping
  //
  public void invoke(ServerRequest request)
  {
    String name = request.operation();

    if (!name.equals("aMethod"))
      {
        throw new BAD_OPERATION();
      }

    //
    // 8.3.1: "Unless it calls set_exception, the DIR must call arguments
    // exactly once, even if the operation signature contains no
    // parameters."
    //
    NVList list = orb_.create_list(0);
    request.arguments(list);

    org.omg.CORBA.Object currentObj = null;
    try
      {
        currentObj = orb_.resolve_initial_references("POACurrent");
      }
    catch (UserException ex)
      {
      }

    org.omg.PortableServer.Current current = null;
    if (currentObj != null)
      current = org.omg.PortableServer.CurrentHelper.narrow(currentObj);
    TEST(current != null);

    byte[] oid = null;
    try
      {
        oid = current.get_object_id();
      }
    catch (org.omg.PortableServer.CurrentPackage.NoContext ex)
      {
        throw new RuntimeException();
      }

    String oidString = new String(oid);

    if (compare_)
      TEST(oidString.equals(name_));

    //
    // Disabled since it is CORBA 2.4
    //

    /*
            org.omg.PortableServer.Servant servant = null;
            try
            {
                servant = current.get_servant();
            }
            catch(org.omg.PortableServer.CurrentPackage.NoContext ex)
            {
                throw new RuntimeException();
            }
            TEST(servant == this);
    */
    if (defaultServant_)
      {
        POA poa = null;
        try
          {
            poa = current.get_POA();
          }
        catch (org.omg.PortableServer.CurrentPackage.NoContext ex)
          {
            throw new RuntimeException();
          }

        byte[] servantId = null;
        try
          {
            servantId = poa.servant_to_id(this);
          }
        catch (ServantNotActive ex)
          {
            throw new RuntimeException();
          }
        catch (WrongPolicy ex)
          {
            throw new RuntimeException();
          }
        TEST(servantId.length == oid.length);
        TEST(servantId.equals(oid));
      }
  }

  static final String[] interfaces_ = { "IDL:Test:1.0" };

  public String[] _all_interfaces(POA poa, byte[] oid)
  {
    return interfaces_;
  }
}
