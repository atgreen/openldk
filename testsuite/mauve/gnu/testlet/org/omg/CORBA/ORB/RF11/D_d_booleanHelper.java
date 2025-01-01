// Tags: not-a-test
// Copyright (c) 2000, 2001 NEC Corporation.

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

package gnu.testlet.org.omg.CORBA.ORB.RF11;

public abstract class D_d_booleanHelper
{
  private static String _id = "IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/D_d_boolean:1.0";

  public static void insert(org.omg.CORBA.Any a, D_d_boolean that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  public static D_d_boolean extract(org.omg.CORBA.Any a)
  {
    return read(a.create_input_stream());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;

  public static synchronized org.omg.CORBA.TypeCode type()
  {
    if (__typeCode == null)
      {
        org.omg.CORBA.TypeCode _disTypeCode0;
        _disTypeCode0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_boolean);

        org.omg.CORBA.UnionMember[] _members0 =
          new org.omg.CORBA.UnionMember[ 2 ];
        org.omg.CORBA.TypeCode _tcOf_members0;
        org.omg.CORBA.Any _anyOf_members0;

        // Branch for l1
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_boolean((boolean) true);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_long);
        _members0 [ 0 ] =
          new org.omg.CORBA.UnionMember("l1", _anyOf_members0, _tcOf_members0,
                                        null
                                       );

        // Branch for l2
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_boolean((boolean) false);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_long);
        _members0 [ 1 ] =
          new org.omg.CORBA.UnionMember("l2", _anyOf_members0, _tcOf_members0,
                                        null
                                       );
        __typeCode =
          org.omg.CORBA.ORB.init().create_union_tc(D_d_booleanHelper.id(),
                                                   "D_d_boolean",
                                                   _disTypeCode0, _members0
                                                  );
      }
    return __typeCode;
  }

  public static String id()
  {
    return _id;
  }

  public static D_d_boolean read(org.omg.CORBA.portable.InputStream istream)
  {
    D_d_boolean value = new D_d_boolean();
    boolean _dis0 = false;
    _dis0 = istream.read_boolean();
    if (_dis0)
      {
        int _l1 = (int) 0;
        _l1 = istream.read_long();
        value.l1(_l1);
      }
    else
      {
        int _l2 = (int) 0;
        _l2 = istream.read_long();
        value.l2(_l2);
      }
    return value;
  }

  public static void write(org.omg.CORBA.portable.OutputStream ostream,
                           D_d_boolean value
                          )
  {
    ostream.write_boolean(value.discriminator());
    if (value.discriminator())
      {
        ostream.write_long(value.l1());
      }
    else
      {
        ostream.write_long(value.l2());
      }
  }
}
