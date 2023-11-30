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

public abstract class C_unionHelper
{
  private static String _id = "IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/C_union:1.0";

  public static void insert(org.omg.CORBA.Any a, C_union that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  public static C_union extract(org.omg.CORBA.Any a)
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
          new org.omg.CORBA.UnionMember[ 12 ];
        org.omg.CORBA.TypeCode _tcOf_members0;
        org.omg.CORBA.Any _anyOf_members0;

        // Branch for e_short
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 1);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_short);
        _members0 [ 0 ] =
          new org.omg.CORBA.UnionMember("e_short", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_ushort
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 2);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_ushort);
        _members0 [ 1 ] =
          new org.omg.CORBA.UnionMember("e_ushort", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_long
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 3);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_long);
        _members0 [ 2 ] =
          new org.omg.CORBA.UnionMember("e_long", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_ulong
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 4);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_ulong);
        _members0 [ 3 ] =
          new org.omg.CORBA.UnionMember("e_ulong", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_float
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 5);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_float);
        _members0 [ 4 ] =
          new org.omg.CORBA.UnionMember("e_float", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_double
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 6);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_double);
        _members0 [ 5 ] =
          new org.omg.CORBA.UnionMember("e_double", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_char
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 7);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_char);
        _members0 [ 6 ] =
          new org.omg.CORBA.UnionMember("e_char", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_boolean
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 8);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_boolean);
        _members0 [ 7 ] =
          new org.omg.CORBA.UnionMember("e_boolean", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_octet
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 9);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_octet);
        _members0 [ 8 ] =
          new org.omg.CORBA.UnionMember("e_octet", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_any
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 10);
        _tcOf_members0 =
          org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_any);
        _members0 [ 9 ] =
          new org.omg.CORBA.UnionMember("e_any", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_string
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 11);
        _tcOf_members0 = org.omg.CORBA.ORB.init().create_string_tc(0);
        _members0 [ 10 ] =
          new org.omg.CORBA.UnionMember("e_string", _anyOf_members0,
                                        _tcOf_members0, null
                                       );

        // Branch for e_Object
        _anyOf_members0 = org.omg.CORBA.ORB.init().create_any();
        _anyOf_members0.insert_long((int) 12);
        _tcOf_members0 = org.omg.CORBA.ObjectHelper.type();
        _members0 [ 11 ] =
          new org.omg.CORBA.UnionMember("e_Object", _anyOf_members0,
                                        _tcOf_members0, null
                                       );
        __typeCode =
          org.omg.CORBA.ORB.init().create_union_tc(C_unionHelper.id(),
                                                   "C_union", _disTypeCode0,
                                                   _members0
                                                  );
      }
    return __typeCode;
  }

  public static String id()
  {
    return _id;
  }

  public static C_union read(org.omg.CORBA.portable.InputStream istream)
  {
    C_union value = new C_union();
    int _dis0 = (int) 0;
    _dis0 = istream.read_long();
    switch (_dis0)
      {
        case 1 :

          short _e_short = (short) 0;
          _e_short = istream.read_short();
          value.e_short(_e_short);
          break;

        case 2 :

          short _e_ushort = (short) 0;
          _e_ushort = istream.read_ushort();
          value.e_ushort(_e_ushort);
          break;

        case 3 :

          int _e_long = (int) 0;
          _e_long = istream.read_long();
          value.e_long(_e_long);
          break;

        case 4 :

          int _e_ulong = (int) 0;
          _e_ulong = istream.read_ulong();
          value.e_ulong(_e_ulong);
          break;

        case 5 :

          float _e_float = (float) 0;
          _e_float = istream.read_float();
          value.e_float(_e_float);
          break;

        case 6 :

          double _e_double = (double) 0;
          _e_double = istream.read_double();
          value.e_double(_e_double);
          break;

        case 7 :

          char _e_char = (char) 0;
          _e_char = istream.read_char();
          value.e_char(_e_char);
          break;

        case 8 :

          boolean _e_boolean = false;
          _e_boolean = istream.read_boolean();
          value.e_boolean(_e_boolean);
          break;

        case 9 :

          byte _e_octet = (byte) 0;
          _e_octet = istream.read_octet();
          value.e_octet(_e_octet);
          break;

        case 10 :

          org.omg.CORBA.Any _e_any = null;
          _e_any = istream.read_any();
          value.e_any(_e_any);
          break;

        case 11 :

          String _e_string = null;
          _e_string = istream.read_string();
          value.e_string(_e_string);
          break;

        case 12 :

          org.omg.CORBA.Object _e_Object = null;
          _e_Object = org.omg.CORBA.ObjectHelper.read(istream);
          value.e_Object(_e_Object);
          break;

        default :
          throw new org.omg.CORBA.BAD_OPERATION();
      }
    return value;
  }

  public static void write(org.omg.CORBA.portable.OutputStream ostream,
                           C_union value
                          )
  {
    ostream.write_long(value.discriminator());
    switch (value.discriminator())
      {
        case 1 :
          ostream.write_short(value.e_short());
          break;

        case 2 :
          ostream.write_ushort(value.e_ushort());
          break;

        case 3 :
          ostream.write_long(value.e_long());
          break;

        case 4 :
          ostream.write_ulong(value.e_ulong());
          break;

        case 5 :
          ostream.write_float(value.e_float());
          break;

        case 6 :
          ostream.write_double(value.e_double());
          break;

        case 7 :
          ostream.write_char(value.e_char());
          break;

        case 8 :
          ostream.write_boolean(value.e_boolean());
          break;

        case 9 :
          ostream.write_octet(value.e_octet());
          break;

        case 10 :
          ostream.write_any(value.e_any());
          break;

        case 11 :
          ostream.write_string(value.e_string());
          break;

        case 12 :
          org.omg.CORBA.ObjectHelper.write(ostream, value.e_Object());
          break;

        default :
          throw new org.omg.CORBA.BAD_OPERATION();
      }
  }
}
