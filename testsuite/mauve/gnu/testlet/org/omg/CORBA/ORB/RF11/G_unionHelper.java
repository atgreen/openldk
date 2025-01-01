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

public abstract class G_unionHelper
{
  private static String _id = "IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/G_union:1.0";

  public static void insert(org.omg.CORBA.Any a, G_union that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  public static G_union extract(org.omg.CORBA.Any a)
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
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_long);

        org.omg.CORBA.UnionMember[] _members0 =
          new org.omg.CORBA.UnionMember[ 4 ];
        org.omg.CORBA.TypeCode _tcOf_members0;
        org.omg.CORBA.Any _anyOf_members0;

        // Branch for e_e_struct
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 1);
        _tcOf_members0 = E_structHelper.type();
        _members0 [ 0 ] =
          new org.omg.CORBA.UnionMember("e_e_struct", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_e_union
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 2);
        _tcOf_members0 = E_unionHelper.type();
        _members0 [ 1 ] =
          new org.omg.CORBA.UnionMember("e_e_union", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_e_sequence
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 3);
        _tcOf_members0 = BHelper.type();
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().create_alias_tc(E_sequenceHelper.id(),
                                                   "E_sequence", _tcOf_members0
                                                  );
        _members0 [ 2 ] =
          new org.omg.CORBA.UnionMember("e_e_sequence", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_e_array
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 4);
        _tcOf_members0 = BHelper.type();
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().create_alias_tc(E_arrayHelper.id(),
                                                   "E_array", _tcOf_members0
                                                  );
        _members0 [ 3 ] =
          new org.omg.CORBA.UnionMember("e_e_array", _anyOf_members0,
                                        _tcOf_members0, null
                                       );
        __typeCode =
          org.omg.CORBA.ORB.init().create_union_tc(G_unionHelper.id(),
                                                   "G_union", _disTypeCode0,
                                                   _members0
                                                  );
      }
    return __typeCode;
  }

  public static String id()
  {
    return _id;
  }

  public static G_union read(org.omg.CORBA.portable.InputStream istream)
  {
    G_union value = new G_union();
    int _dis0 = (int) 0;
    _dis0 = istream.read_long();
    switch (_dis0)
      {
        case 1 :

          E_struct _e_e_struct = null;
          _e_e_struct = E_structHelper.read(istream);
          value.e_e_struct(_e_e_struct);
          break;

        case 2 :

          E_union _e_e_union = null;
          _e_e_union = E_unionHelper.read(istream);
          value.e_e_union(_e_e_union);
          break;

        case 3 :

          B[] _e_e_sequence = null;
          _e_e_sequence = E_sequenceHelper.read(istream);
          value.e_e_sequence(_e_e_sequence);
          break;

        case 4 :

          B[] _e_e_array = null;
          _e_e_array = E_arrayHelper.read(istream);
          value.e_e_array(_e_e_array);
          break;

        default :
          throw new org.omg.CORBA.BAD_OPERATION();
      }
    return value;
  }

  public static void write(org.omg.CORBA.portable.OutputStream ostream,
                           G_union value
                          )
  {
    ostream.write_long(value.discriminator());
    switch (value.discriminator())
      {
        case 1 :
          E_structHelper.write(ostream, value.e_e_struct());
          break;

        case 2 :
          E_unionHelper.write(ostream, value.e_e_union());
          break;

        case 3 :
          E_sequenceHelper.write(ostream, value.e_e_sequence());
          break;

        case 4 :
          E_arrayHelper.write(ostream, value.e_e_array());
          break;

        default :
          throw new org.omg.CORBA.BAD_OPERATION();
      }
  }
}
