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

public abstract class D_d_ushortHelper
{
  private static String _id = "IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/D_d_ushort:1.0";

  public static void insert(org.omg.CORBA.Any a, D_d_ushort that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  public static D_d_ushort extract(org.omg.CORBA.Any a)
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
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_ushort);

        org.omg.CORBA.UnionMember[] _members0 =
          new org.omg.CORBA.UnionMember[ 2 ];
        org.omg.CORBA.TypeCode _tcOf_members0;
        org.omg.CORBA.Any _anyOf_members0;

        // Branch for l1
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_ushort((short) 1);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_long);
        _members0 [ 0 ] =
          new org.omg.CORBA.UnionMember("l1", _anyOf_members0, _tcOf_members0,
                                        null
                                       );

        // Branch for l2
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_ushort((short) 2);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_long);
        _members0 [ 1 ] =
          new org.omg.CORBA.UnionMember("l2", _anyOf_members0, _tcOf_members0,
                                        null
                                       );
        __typeCode =
          org.omg.CORBA.ORB.init().create_union_tc(D_d_ushortHelper.id(),
                                                   "D_d_ushort", _disTypeCode0,
                                                   _members0
                                                  );
      }
    return __typeCode;
  }

  public static String id()
  {
    return _id;
  }

  public static D_d_ushort read(org.omg.CORBA.portable.InputStream istream)
  {
    D_d_ushort value = new D_d_ushort();
    short _dis0 = (short) 0;
    _dis0 = istream.read_ushort();
    switch (_dis0)
      {
        case 1 :

          int _l1 = (int) 0;
          _l1 = istream.read_long();
          value.l1(_l1);
          break;

        case 2 :

          int _l2 = (int) 0;
          _l2 = istream.read_long();
          value.l2(_l2);
          break;

        default :
          throw new org.omg.CORBA.BAD_OPERATION();
      }
    return value;
  }

  public static void write(org.omg.CORBA.portable.OutputStream ostream,
                           D_d_ushort value
                          )
  {
    ostream.write_ushort(value.discriminator());
    switch (value.discriminator())
      {
        case 1 :
          ostream.write_long(value.l1());
          break;

        case 2 :
          ostream.write_long(value.l2());
          break;

        default :
          throw new org.omg.CORBA.BAD_OPERATION();
      }
  }
}
