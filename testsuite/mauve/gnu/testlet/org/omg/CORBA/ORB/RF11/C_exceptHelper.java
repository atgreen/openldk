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

public abstract class C_exceptHelper
{
  private static String _id = "IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/C_except:1.0";

  public static void insert(org.omg.CORBA.Any a, C_except that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  public static C_except extract(org.omg.CORBA.Any a)
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
                  new org.omg.CORBA.StructMember[ 37 ];
                org.omg.CORBA.TypeCode _tcOf_members0 = null;
                _tcOf_members0 = C_structHelper.type();
                _members0 [ 0 ] =
                  new org.omg.CORBA.StructMember("v1", _tcOf_members0, null);
                _tcOf_members0 = C_unionHelper.type();
                _members0 [ 1 ] =
                  new org.omg.CORBA.StructMember("v2", _tcOf_members0, null);
                _tcOf_members0 = C_unionHelper.type();
                _members0 [ 2 ] =
                  new org.omg.CORBA.StructMember("v3", _tcOf_members0, null);
                _tcOf_members0 = C_unionHelper.type();
                _members0 [ 3 ] =
                  new org.omg.CORBA.StructMember("v4", _tcOf_members0, null);
                _tcOf_members0 = C_unionHelper.type();
                _members0 [ 4 ] =
                  new org.omg.CORBA.StructMember("v5", _tcOf_members0, null);
                _tcOf_members0 = C_unionHelper.type();
                _members0 [ 5 ] =
                  new org.omg.CORBA.StructMember("v6", _tcOf_members0, null);
                _tcOf_members0 = C_unionHelper.type();
                _members0 [ 6 ] =
                  new org.omg.CORBA.StructMember("v7", _tcOf_members0, null);
                _tcOf_members0 = C_unionHelper.type();
                _members0 [ 7 ] =
                  new org.omg.CORBA.StructMember("v8", _tcOf_members0, null);
                _tcOf_members0 = C_unionHelper.type();
                _members0 [ 8 ] =
                  new org.omg.CORBA.StructMember("v9", _tcOf_members0, null);
                _tcOf_members0 = C_unionHelper.type();
                _members0 [ 9 ] =
                  new org.omg.CORBA.StructMember("v10", _tcOf_members0, null);
                _tcOf_members0 = C_unionHelper.type();
                _members0 [ 10 ] =
                  new org.omg.CORBA.StructMember("v11", _tcOf_members0, null);
                _tcOf_members0 = C_unionHelper.type();
                _members0 [ 11 ] =
                  new org.omg.CORBA.StructMember("v12", _tcOf_members0, null);
                _tcOf_members0 = C_unionHelper.type();
                _members0 [ 12 ] =
                  new org.omg.CORBA.StructMember("v13", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_short);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_shortHelper.id(),
                                                           "C_sequence_e_short",
                                                           _tcOf_members0
                                                          );
                _members0 [ 13 ] =
                  new org.omg.CORBA.StructMember("v16", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_ushort);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_ushortHelper.id(),
                                                           "C_sequence_e_ushort",
                                                           _tcOf_members0
                                                          );
                _members0 [ 14 ] =
                  new org.omg.CORBA.StructMember("v17", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_long);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_longHelper.id(),
                                                           "C_sequence_e_long",
                                                           _tcOf_members0
                                                          );
                _members0 [ 15 ] =
                  new org.omg.CORBA.StructMember("v18", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_ulong);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_ulongHelper.id(),
                                                           "C_sequence_e_ulong",
                                                           _tcOf_members0
                                                          );
                _members0 [ 16 ] =
                  new org.omg.CORBA.StructMember("v19", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_float);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_floatHelper.id(),
                                                           "C_sequence_e_float",
                                                           _tcOf_members0
                                                          );
                _members0 [ 17 ] =
                  new org.omg.CORBA.StructMember("v20", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_double);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_doubleHelper.id(),
                                                           "C_sequence_e_double",
                                                           _tcOf_members0
                                                          );
                _members0 [ 18 ] =
                  new org.omg.CORBA.StructMember("v21", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_char);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_charHelper.id(),
                                                           "C_sequence_e_char",
                                                           _tcOf_members0
                                                          );
                _members0 [ 19 ] =
                  new org.omg.CORBA.StructMember("v22", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_boolean);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_booleanHelper.id(),
                                                           "C_sequence_e_boolean",
                                                           _tcOf_members0
                                                          );
                _members0 [ 20 ] =
                  new org.omg.CORBA.StructMember("v23", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_octet);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_octetHelper.id(),
                                                           "C_sequence_e_octet",
                                                           _tcOf_members0
                                                          );
                _members0 [ 21 ] =
                  new org.omg.CORBA.StructMember("v24", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_any);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_anyHelper.id(),
                                                           "C_sequence_e_any",
                                                           _tcOf_members0
                                                          );
                _members0 [ 22 ] =
                  new org.omg.CORBA.StructMember("v25", _tcOf_members0, null);
                _tcOf_members0 = org.omg.CORBA.ORB.init().create_string_tc(0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_stringHelper.id(),
                                                           "C_sequence_e_string",
                                                           _tcOf_members0
                                                          );
                _members0 [ 23 ] =
                  new org.omg.CORBA.StructMember("v26", _tcOf_members0, null);
                _tcOf_members0 = org.omg.CORBA.ObjectHelper.type();
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_sequence_tc(0, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_sequence_e_ObjectHelper.id(),
                                                           "C_sequence_e_Object",
                                                           _tcOf_members0
                                                          );
                _members0 [ 24 ] =
                  new org.omg.CORBA.StructMember("v27", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_short);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_shortHelper.id(),
                                                           "C_array_e_short",
                                                           _tcOf_members0
                                                          );
                _members0 [ 25 ] =
                  new org.omg.CORBA.StructMember("v30", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_ushort);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_ushortHelper.id(),
                                                           "C_array_e_ushort",
                                                           _tcOf_members0
                                                          );
                _members0 [ 26 ] =
                  new org.omg.CORBA.StructMember("v31", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_long);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_longHelper.id(),
                                                           "C_array_e_long",
                                                           _tcOf_members0
                                                          );
                _members0 [ 27 ] =
                  new org.omg.CORBA.StructMember("v32", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_ulong);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_ulongHelper.id(),
                                                           "C_array_e_ulong",
                                                           _tcOf_members0
                                                          );
                _members0 [ 28 ] =
                  new org.omg.CORBA.StructMember("v33", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_float);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_floatHelper.id(),
                                                           "C_array_e_float",
                                                           _tcOf_members0
                                                          );
                _members0 [ 29 ] =
                  new org.omg.CORBA.StructMember("v34", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_double);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_doubleHelper.id(),
                                                           "C_array_e_double",
                                                           _tcOf_members0
                                                          );
                _members0 [ 30 ] =
                  new org.omg.CORBA.StructMember("v35", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_char);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_charHelper.id(),
                                                           "C_array_e_char",
                                                           _tcOf_members0
                                                          );
                _members0 [ 31 ] =
                  new org.omg.CORBA.StructMember("v36", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_boolean);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_booleanHelper.id(),
                                                           "C_array_e_boolean",
                                                           _tcOf_members0
                                                          );
                _members0 [ 32 ] =
                  new org.omg.CORBA.StructMember("v37", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_octet);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_octetHelper.id(),
                                                           "C_array_e_octet",
                                                           _tcOf_members0
                                                          );
                _members0 [ 33 ] =
                  new org.omg.CORBA.StructMember("v38", _tcOf_members0, null);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().get_primitive_tc(org.omg.CORBA.TCKind.tk_any);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_anyHelper.id(),
                                                           "C_array_e_any",
                                                           _tcOf_members0
                                                          );
                _members0 [ 34 ] =
                  new org.omg.CORBA.StructMember("v39", _tcOf_members0, null);
                _tcOf_members0 = org.omg.CORBA.ORB.init().create_string_tc(0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_stringHelper.id(),
                                                           "C_array_e_string",
                                                           _tcOf_members0
                                                          );
                _members0 [ 35 ] =
                  new org.omg.CORBA.StructMember("v40", _tcOf_members0, null);
                _tcOf_members0 = org.omg.CORBA.ObjectHelper.type();
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_array_tc(2, _tcOf_members0);
                _tcOf_members0 =
                  org.omg.CORBA.ORB.init().create_alias_tc(C_array_e_ObjectHelper.id(),
                                                           "C_array_e_Object",
                                                           _tcOf_members0
                                                          );
                _members0 [ 36 ] =
                  new org.omg.CORBA.StructMember("v41", _tcOf_members0, null);
                __typeCode =
                  org.omg.CORBA.ORB.init().create_struct_tc(C_exceptHelper.id(),
                                                            "C_except",
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

  public static C_except read(org.omg.CORBA.portable.InputStream istream)
  {
    C_except value = new C_except();

    // read and discard the repository ID
    istream.read_string();
    value.v1 = C_structHelper.read(istream);
    value.v2 = C_unionHelper.read(istream);
    value.v3 = C_unionHelper.read(istream);
    value.v4 = C_unionHelper.read(istream);
    value.v5 = C_unionHelper.read(istream);
    value.v6 = C_unionHelper.read(istream);
    value.v7 = C_unionHelper.read(istream);
    value.v8 = C_unionHelper.read(istream);
    value.v9 = C_unionHelper.read(istream);
    value.v10 = C_unionHelper.read(istream);
    value.v11 = C_unionHelper.read(istream);
    value.v12 = C_unionHelper.read(istream);
    value.v13 = C_unionHelper.read(istream);
    value.v16 = C_sequence_e_shortHelper.read(istream);
    value.v17 = C_sequence_e_ushortHelper.read(istream);
    value.v18 = C_sequence_e_longHelper.read(istream);
    value.v19 = C_sequence_e_ulongHelper.read(istream);
    value.v20 = C_sequence_e_floatHelper.read(istream);
    value.v21 = C_sequence_e_doubleHelper.read(istream);
    value.v22 = C_sequence_e_charHelper.read(istream);
    value.v23 = C_sequence_e_booleanHelper.read(istream);
    value.v24 = C_sequence_e_octetHelper.read(istream);
    value.v25 = C_sequence_e_anyHelper.read(istream);
    value.v26 = C_sequence_e_stringHelper.read(istream);
    value.v27 = C_sequence_e_ObjectHelper.read(istream);
    value.v30 = C_array_e_shortHelper.read(istream);
    value.v31 = C_array_e_ushortHelper.read(istream);
    value.v32 = C_array_e_longHelper.read(istream);
    value.v33 = C_array_e_ulongHelper.read(istream);
    value.v34 = C_array_e_floatHelper.read(istream);
    value.v35 = C_array_e_doubleHelper.read(istream);
    value.v36 = C_array_e_charHelper.read(istream);
    value.v37 = C_array_e_booleanHelper.read(istream);
    value.v38 = C_array_e_octetHelper.read(istream);
    value.v39 = C_array_e_anyHelper.read(istream);
    value.v40 = C_array_e_stringHelper.read(istream);
    value.v41 = C_array_e_ObjectHelper.read(istream);
    return value;
  }

  public static void write(org.omg.CORBA.portable.OutputStream ostream,
                           C_except value
                          )
  {
    // write the repository ID
    ostream.write_string(id());
    C_structHelper.write(ostream, value.v1);
    C_unionHelper.write(ostream, value.v2);
    C_unionHelper.write(ostream, value.v3);
    C_unionHelper.write(ostream, value.v4);
    C_unionHelper.write(ostream, value.v5);
    C_unionHelper.write(ostream, value.v6);
    C_unionHelper.write(ostream, value.v7);
    C_unionHelper.write(ostream, value.v8);
    C_unionHelper.write(ostream, value.v9);
    C_unionHelper.write(ostream, value.v10);
    C_unionHelper.write(ostream, value.v11);
    C_unionHelper.write(ostream, value.v12);
    C_unionHelper.write(ostream, value.v13);
    C_sequence_e_shortHelper.write(ostream, value.v16);
    C_sequence_e_ushortHelper.write(ostream, value.v17);
    C_sequence_e_longHelper.write(ostream, value.v18);
    C_sequence_e_ulongHelper.write(ostream, value.v19);
    C_sequence_e_floatHelper.write(ostream, value.v20);
    C_sequence_e_doubleHelper.write(ostream, value.v21);
    C_sequence_e_charHelper.write(ostream, value.v22);
    C_sequence_e_booleanHelper.write(ostream, value.v23);
    C_sequence_e_octetHelper.write(ostream, value.v24);
    C_sequence_e_anyHelper.write(ostream, value.v25);
    C_sequence_e_stringHelper.write(ostream, value.v26);
    C_sequence_e_ObjectHelper.write(ostream, value.v27);
    C_array_e_shortHelper.write(ostream, value.v30);
    C_array_e_ushortHelper.write(ostream, value.v31);
    C_array_e_longHelper.write(ostream, value.v32);
    C_array_e_ulongHelper.write(ostream, value.v33);
    C_array_e_floatHelper.write(ostream, value.v34);
    C_array_e_doubleHelper.write(ostream, value.v35);
    C_array_e_charHelper.write(ostream, value.v36);
    C_array_e_booleanHelper.write(ostream, value.v37);
    C_array_e_octetHelper.write(ostream, value.v38);
    C_array_e_anyHelper.write(ostream, value.v39);
    C_array_e_stringHelper.write(ostream, value.v40);
    C_array_e_ObjectHelper.write(ostream, value.v41);
  }
}
