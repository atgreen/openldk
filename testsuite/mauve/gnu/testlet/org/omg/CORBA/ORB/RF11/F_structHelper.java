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


// F
package gnu.testlet.org.omg.CORBA.ORB.RF11;

public abstract class F_structHelper
{
  private static String _id = "IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/F_struct:1.0";

  public static void insert(org.omg.CORBA.Any a, F_struct that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  public static F_struct extract(org.omg.CORBA.Any a)
  {
    return read(a.create_input_stream());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  private static boolean __active = false;

  public static synchronized org.omg.CORBA.TypeCode type()
  {
    if (__typeCode == null)
      {
        synchronized (org.omg.CORBA.TypeCode.class)
          {
            if (__typeCode == null)
              {
                if (__active)
                  {
                    return org.omg.CORBA.ORB.init().create_recursive_tc(_id);
                  }
                __active = true;

                org.omg.CORBA.StructMember[] _members0 =
                  new org.omg.CORBA.StructMember[ 26 ];
                org.omg.CORBA.TypeCode _tcOf_members0 = null;
                _tcOf_members0 = C_structHelper.type();
                _members0 [ 0 ] =
                  new org.omg.CORBA.StructMember("e_c_struct", _tcOf_members0,
                                                 null
                                                );
                _tcOf_members0 = C_unionHelper.type();
                _members0 [ 1 ] =
                  new org.omg.CORBA.StructMember("e_c_union", _tcOf_members0,
                                                 null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_short);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_shortHelper.id(),
                                                           "C_sequence_e_short",
                                                           _tcOf_members0
                                                          );
                _members0 [ 2 ] =
                  new org.omg.CORBA.StructMember("e_c_sequence_e_short",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_ushort);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_ushortHelper.id(),
                                                           "C_sequence_e_ushort",
                                                           _tcOf_members0
                                                          );
                _members0 [ 3 ] =
                  new org.omg.CORBA.StructMember("e_c_sequence_e_ushort",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_long);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_longHelper.id(),
                                                           "C_sequence_e_long",
                                                           _tcOf_members0
                                                          );
                _members0 [ 4 ] =
                  new org.omg.CORBA.StructMember("e_c_sequence_e_long",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_ulong);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_ulongHelper.id(),
                                                           "C_sequence_e_ulong",
                                                           _tcOf_members0
                                                          );
                _members0 [ 5 ] =
                  new org.omg.CORBA.StructMember("e_c_sequence_e_ulong",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_float);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_floatHelper.id(),
                                                           "C_sequence_e_float",
                                                           _tcOf_members0
                                                          );
                _members0 [ 6 ] =
                  new org.omg.CORBA.StructMember("e_c_sequence_e_float",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_double);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_doubleHelper.id(),
                                                           "C_sequence_e_double",
                                                           _tcOf_members0
                                                          );
                _members0 [ 7 ] =
                  new org.omg.CORBA.StructMember("e_c_sequence_e_double",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_char);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_charHelper.id(),
                                                           "C_sequence_e_char",
                                                           _tcOf_members0
                                                          );
                _members0 [ 8 ] =
                  new org.omg.CORBA.StructMember("e_c_sequence_e_char",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_boolean);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_booleanHelper.id(),
                                                           "C_sequence_e_boolean",
                                                           _tcOf_members0
                                                          );
                _members0 [ 9 ] =
                  new org.omg.CORBA.StructMember("e_c_sequence_e_boolean",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_octet);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_octetHelper.id(),
                                                           "C_sequence_e_octet",
                                                           _tcOf_members0
                                                          );
                _members0 [ 10 ] =
                  new org.omg.CORBA.StructMember("e_c_sequence_e_octet",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_any);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_anyHelper.id(),
                                                           "C_sequence_e_any",
                                                           _tcOf_members0
                                                          );
                _members0 [ 11 ] =
                  new org.omg.CORBA.StructMember("e_c_sequence_e_any",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 = org.omg.CORBA.ORB.init().create_string_tc(0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_stringHelper.id(),
                                                           "C_sequence_e_string",
                                                           _tcOf_members0
                                                          );
                _members0 [ 12 ] =
                  new org.omg.CORBA.StructMember("e_c_sequence_e_string",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 = org.omg.CORBA.ObjectHelper.type();
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_ObjectHelper.id(),
                                                           "C_sequence_e_Object",
                                                           _tcOf_members0
                                                          );
                _members0 [ 13 ] =
                  new org.omg.CORBA.StructMember("e_c_sequence_e_Object",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_short);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_shortHelper.id(),
                                                           "C_array_e_short",
                                                           _tcOf_members0
                                                          );
                _members0 [ 14 ] =
                  new org.omg.CORBA.StructMember("e_c_array_e_short",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_ushort);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_ushortHelper.id(),
                                                           "C_array_e_ushort",
                                                           _tcOf_members0
                                                          );
                _members0 [ 15 ] =
                  new org.omg.CORBA.StructMember("e_c_array_e_ushort",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_long);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_longHelper.id(),
                                                           "C_array_e_long",
                                                           _tcOf_members0
                                                          );
                _members0 [ 16 ] =
                  new org.omg.CORBA.StructMember("e_c_array_e_long",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_ulong);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_ulongHelper.id(),
                                                           "C_array_e_ulong",
                                                           _tcOf_members0
                                                          );
                _members0 [ 17 ] =
                  new org.omg.CORBA.StructMember("e_c_array_e_ulong",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_float);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_floatHelper.id(),
                                                           "C_array_e_float",
                                                           _tcOf_members0
                                                          );
                _members0 [ 18 ] =
                  new org.omg.CORBA.StructMember("e_c_array_e_float",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_double);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_doubleHelper.id(),
                                                           "C_array_e_double",
                                                           _tcOf_members0
                                                          );
                _members0 [ 19 ] =
                  new org.omg.CORBA.StructMember("e_c_array_e_double",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_char);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_charHelper.id(),
                                                           "C_array_e_char",
                                                           _tcOf_members0
                                                          );
                _members0 [ 20 ] =
                  new org.omg.CORBA.StructMember("e_c_array_e_char",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_boolean);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_booleanHelper.id(),
                                                           "C_array_e_boolean",
                                                           _tcOf_members0
                                                          );
                _members0 [ 21 ] =
                  new org.omg.CORBA.StructMember("e_c_array_e_boolean",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_octet);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_octetHelper.id(),
                                                           "C_array_e_octet",
                                                           _tcOf_members0
                                                          );
                _members0 [ 22 ] =
                  new org.omg.CORBA.StructMember("e_c_array_e_octet",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_any);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_anyHelper.id(),
                                                           "C_array_e_any",
                                                           _tcOf_members0
                                                          );
                _members0 [ 23 ] =
                  new org.omg.CORBA.StructMember("e_c_array_e_any",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 = org.omg.CORBA.ORB.init().create_string_tc(0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_stringHelper.id(),
                                                           "C_array_e_string",
                                                           _tcOf_members0
                                                          );
                _members0 [ 24 ] =
                  new org.omg.CORBA.StructMember("e_c_array_e_string",
                                                 _tcOf_members0, null
                                                );
                _tcOf_members0 = org.omg.CORBA.ObjectHelper.type();
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_ObjectHelper.id(),
                                                           "C_array_e_Object",
                                                           _tcOf_members0
                                                          );
                _members0 [ 25 ] =
                  new org.omg.CORBA.StructMember("e_c_array_e_Object",
                                                 _tcOf_members0, null
                                                );
                __typeCode =
                  org.omg.CORBA.ORB.init().create_struct_tc(F_structHelper.id(),
                                                            "F_struct",
                                                            _members0
                                                           );
                __active = false;
              }
          }
      }
    return __typeCode;
  }

  public static String id()
  {
    return _id;
  }

  public static F_struct read(org.omg.CORBA.portable.InputStream istream)
  {
    F_struct value = new F_struct();
    value.e_c_struct = C_structHelper.read(istream);
    value.e_c_union = C_unionHelper.read(istream);
    value.e_c_sequence_e_short = C_sequence_e_shortHelper.read(istream);
    value.e_c_sequence_e_ushort = C_sequence_e_ushortHelper.read(istream);
    value.e_c_sequence_e_long = C_sequence_e_longHelper.read(istream);
    value.e_c_sequence_e_ulong = C_sequence_e_ulongHelper.read(istream);
    value.e_c_sequence_e_float = C_sequence_e_floatHelper.read(istream);
    value.e_c_sequence_e_double = C_sequence_e_doubleHelper.read(istream);
    value.e_c_sequence_e_char = C_sequence_e_charHelper.read(istream);
    value.e_c_sequence_e_boolean = C_sequence_e_booleanHelper.read(istream);
    value.e_c_sequence_e_octet = C_sequence_e_octetHelper.read(istream);
    value.e_c_sequence_e_any = C_sequence_e_anyHelper.read(istream);
    value.e_c_sequence_e_string = C_sequence_e_stringHelper.read(istream);
    value.e_c_sequence_e_Object = C_sequence_e_ObjectHelper.read(istream);
    value.e_c_array_e_short = C_array_e_shortHelper.read(istream);
    value.e_c_array_e_ushort = C_array_e_ushortHelper.read(istream);
    value.e_c_array_e_long = C_array_e_longHelper.read(istream);
    value.e_c_array_e_ulong = C_array_e_ulongHelper.read(istream);
    value.e_c_array_e_float = C_array_e_floatHelper.read(istream);
    value.e_c_array_e_double = C_array_e_doubleHelper.read(istream);
    value.e_c_array_e_char = C_array_e_charHelper.read(istream);
    value.e_c_array_e_boolean = C_array_e_booleanHelper.read(istream);
    value.e_c_array_e_octet = C_array_e_octetHelper.read(istream);
    value.e_c_array_e_any = C_array_e_anyHelper.read(istream);
    value.e_c_array_e_string = C_array_e_stringHelper.read(istream);
    value.e_c_array_e_Object = C_array_e_ObjectHelper.read(istream);
    return value;
  }

  public static void write(org.omg.CORBA.portable.OutputStream ostream,
                           F_struct value
                          )
  {
    C_structHelper.write(ostream, value.e_c_struct);
    C_unionHelper.write(ostream, value.e_c_union);
    C_sequence_e_shortHelper.write(ostream, value.e_c_sequence_e_short);
    C_sequence_e_ushortHelper.write(ostream, value.e_c_sequence_e_ushort);
    C_sequence_e_longHelper.write(ostream, value.e_c_sequence_e_long);
    C_sequence_e_ulongHelper.write(ostream, value.e_c_sequence_e_ulong);
    C_sequence_e_floatHelper.write(ostream, value.e_c_sequence_e_float);
    C_sequence_e_doubleHelper.write(ostream, value.e_c_sequence_e_double);
    C_sequence_e_charHelper.write(ostream, value.e_c_sequence_e_char);
    C_sequence_e_booleanHelper.write(ostream, value.e_c_sequence_e_boolean);
    C_sequence_e_octetHelper.write(ostream, value.e_c_sequence_e_octet);
    C_sequence_e_anyHelper.write(ostream, value.e_c_sequence_e_any);
    C_sequence_e_stringHelper.write(ostream, value.e_c_sequence_e_string);
    C_sequence_e_ObjectHelper.write(ostream, value.e_c_sequence_e_Object);
    C_array_e_shortHelper.write(ostream, value.e_c_array_e_short);
    C_array_e_ushortHelper.write(ostream, value.e_c_array_e_ushort);
    C_array_e_longHelper.write(ostream, value.e_c_array_e_long);
    C_array_e_ulongHelper.write(ostream, value.e_c_array_e_ulong);
    C_array_e_floatHelper.write(ostream, value.e_c_array_e_float);
    C_array_e_doubleHelper.write(ostream, value.e_c_array_e_double);
    C_array_e_charHelper.write(ostream, value.e_c_array_e_char);
    C_array_e_booleanHelper.write(ostream, value.e_c_array_e_boolean);
    C_array_e_octetHelper.write(ostream, value.e_c_array_e_octet);
    C_array_e_anyHelper.write(ostream, value.e_c_array_e_any);
    C_array_e_stringHelper.write(ostream, value.e_c_array_e_string);
    C_array_e_ObjectHelper.write(ostream, value.e_c_array_e_Object);
  }
}
