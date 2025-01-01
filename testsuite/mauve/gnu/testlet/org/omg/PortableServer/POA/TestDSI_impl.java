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

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.NVList;
import org.omg.CORBA.ORB;
import org.omg.CORBA.ServerRequest;
import org.omg.PortableServer.DynamicImplementation;
import org.omg.PortableServer.POA;

class TestDSI_impl
  extends org.omg.PortableServer.DynamicImplementation
{
  //
  // From TestBase (no multiple inheritance)
  //
  public static void TEST(boolean expr)
  {
    if (!expr)
      throw new TestException();
  }

  protected ORB orb_;
  protected POA poa_;
  protected org.omg.PortableServer.Current current_;
  protected String name_;
  protected boolean compare_;

  TestDSI_impl(ORB orb, String name, boolean compare)
  {
    org.omg.CORBA.Object currentObj = null;

    try
      {
        currentObj = orb.resolve_initial_references("POACurrent");
      }
    catch (org.omg.CORBA.ORBPackage.InvalidName ex)
      {
      }

    TEST(currentObj != null);
    current_ = org.omg.PortableServer.CurrentHelper.narrow(currentObj);
    TEST(current_ != null);
  }

  TestDSI_impl(ORB orb, POA poa)
  {
    orb_ = orb;
    poa_ = poa;
    name_ = "";

    org.omg.CORBA.Object currentObj = null;

    try
      {
        currentObj = orb.resolve_initial_references("POACurrent");
      }
    catch (org.omg.CORBA.ORBPackage.InvalidName ex)
      {
      }

    TEST(currentObj != null);
    current_ = org.omg.PortableServer.CurrentHelper.narrow(currentObj);
    TEST(current_ != null);
  }

  static final String[] interfaces_ = { "IDL:Test:1.0" };

  public String[] _all_interfaces(POA poa, byte[] oid)
  {
    return interfaces_;
  }

  public boolean _is_a(String id)
  {
    if (id.equals("IDL:Test:1:0"))
      return true;
    return super._is_a(id);
  }

  public void invoke(ServerRequest request)
  {
    String name = request.operation();

    if (name.equals("aMethod"))
      {
        NVList list = orb_.create_list(0);
        request.arguments(list);

        if (compare_)
          {
            byte[] oid = null;
            try
              {
                oid = current_.get_object_id();
              }
            catch (org.omg.PortableServer.CurrentPackage.NoContext ex)
              {
                throw new RuntimeException();
              }

            String oidString = new String(oid);

            TEST(oidString.equals(name_));
          }

        return;
      }

    System.err.println("DSI implementation: unknown operation: " + name);

    NVList list = orb_.create_list(0);
    request.arguments(list);

    Any exAny = orb_.create_any();
    exAny.insert_Streamable(new BAD_OPERATIONHolder(new BAD_OPERATION()));

    // BAD_OPERATIONHelper.insert(any, new BAD_OPERATION());
    // Modified by Audrius Meskauskas
    request.set_exception(exAny);
  }

  public POA _default_POA()
  {
    if (poa_ != null)
      return poa_;
    return super._default_POA();
  }
}
