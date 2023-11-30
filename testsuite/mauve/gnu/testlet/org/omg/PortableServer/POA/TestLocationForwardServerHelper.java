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

package gnu.testlet.org.omg.PortableServer.POA;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;

public abstract class TestLocationForwardServerHelper
{
  private static String _id = "IDL:test/poa/TestLocationForwardServer:1.0";

  public static void insert(Any a, TestLocationForwardServer that)
  {
    OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  public static TestLocationForwardServer extract(Any a)
  {
    return read(a.create_input_stream());
  }

  public static TypeCode type()
  {
    return ORB.init().create_interface_tc(TestLocationForwardServerHelper.id(),
                                          "TestLocationForwardServer"
                                         );
  }

  public static String id()
  {
    return _id;
  }

  public static TestLocationForwardServer read(InputStream istream)
  {
    return narrow(istream.read_Object(_TestLocationForwardServerStub.class));
  }

  public static void write(OutputStream ostream, TestLocationForwardServer value)
  {
    ostream.write_Object((org.omg.CORBA.Object) value);
  }

  public static TestLocationForwardServer narrow(org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof TestLocationForwardServer)
      return (TestLocationForwardServer) obj;
    else if (!obj._is_a(id()))
      throw new BAD_PARAM();
    else
      {
        Delegate delegate = ((ObjectImpl) obj)._get_delegate();
        _TestLocationForwardServerStub stub =
          new _TestLocationForwardServerStub();
        stub._set_delegate(delegate);
        return stub;
      }
  }
}
