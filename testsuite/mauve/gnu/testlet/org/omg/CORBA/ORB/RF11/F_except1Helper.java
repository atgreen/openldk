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

public abstract class F_except1Helper
{
  private static String _id = "IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/F_except1:1.0";

  public static void insert(org.omg.CORBA.Any a, F_except1 that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  public static F_except1 extract(org.omg.CORBA.Any a)
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
                  new org.omg.CORBA.StructMember[ 27 ];
                org.omg.CORBA.TypeCode _tcOf_members0 = null;
                _tcOf_members0 = F_structHelper.type();
                _members0 [ 0 ] =
                  new org.omg.CORBA.StructMember("v1", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 1 ] =
                  new org.omg.CORBA.StructMember("v2", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 2 ] =
                  new org.omg.CORBA.StructMember("v3", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 3 ] =
                  new org.omg.CORBA.StructMember("v4", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 4 ] =
                  new org.omg.CORBA.StructMember("v5", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 5 ] =
                  new org.omg.CORBA.StructMember("v6", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 6 ] =
                  new org.omg.CORBA.StructMember("v7", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 7 ] =
                  new org.omg.CORBA.StructMember("v8", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 8 ] =
                  new org.omg.CORBA.StructMember("v9", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 9 ] =
                  new org.omg.CORBA.StructMember("v10", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 10 ] =
                  new org.omg.CORBA.StructMember("v11", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 11 ] =
                  new org.omg.CORBA.StructMember("v12", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 12 ] =
                  new org.omg.CORBA.StructMember("v13", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 13 ] =
                  new org.omg.CORBA.StructMember("v14", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 14 ] =
                  new org.omg.CORBA.StructMember("v15", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 15 ] =
                  new org.omg.CORBA.StructMember("v18", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 16 ] =
                  new org.omg.CORBA.StructMember("v19", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 17 ] =
                  new org.omg.CORBA.StructMember("v20", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 18 ] =
                  new org.omg.CORBA.StructMember("v21", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 19 ] =
                  new org.omg.CORBA.StructMember("v22", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 20 ] =
                  new org.omg.CORBA.StructMember("v23", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 21 ] =
                  new org.omg.CORBA.StructMember("v24", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 22 ] =
                  new org.omg.CORBA.StructMember("v25", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 23 ] =
                  new org.omg.CORBA.StructMember("v26", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 24 ] =
                  new org.omg.CORBA.StructMember("v27", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 25 ] =
                  new org.omg.CORBA.StructMember("v28", _tcOf_members0, null);
                _tcOf_members0 = F_unionHelper.type();
                _members0 [ 26 ] =
                  new org.omg.CORBA.StructMember("v29", _tcOf_members0, null);
                __typeCode =
                  org.omg.CORBA.ORB.init().create_struct_tc(F_except1Helper.id(),
                                                            "F_except1",
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

  public static F_except1 read(org.omg.CORBA.portable.InputStream istream)
  {
    F_except1 value = new F_except1();

    // read and discard the repository ID
    istream.read_string();
    value.v1 = F_structHelper.read(istream);
    value.v2 = F_unionHelper.read(istream);
    value.v3 = F_unionHelper.read(istream);
    value.v4 = F_unionHelper.read(istream);
    value.v5 = F_unionHelper.read(istream);
    value.v6 = F_unionHelper.read(istream);
    value.v7 = F_unionHelper.read(istream);
    value.v8 = F_unionHelper.read(istream);
    value.v9 = F_unionHelper.read(istream);
    value.v10 = F_unionHelper.read(istream);
    value.v11 = F_unionHelper.read(istream);
    value.v12 = F_unionHelper.read(istream);
    value.v13 = F_unionHelper.read(istream);
    value.v14 = F_unionHelper.read(istream);
    value.v15 = F_unionHelper.read(istream);
    value.v18 = F_unionHelper.read(istream);
    value.v19 = F_unionHelper.read(istream);
    value.v20 = F_unionHelper.read(istream);
    value.v21 = F_unionHelper.read(istream);
    value.v22 = F_unionHelper.read(istream);
    value.v23 = F_unionHelper.read(istream);
    value.v24 = F_unionHelper.read(istream);
    value.v25 = F_unionHelper.read(istream);
    value.v26 = F_unionHelper.read(istream);
    value.v27 = F_unionHelper.read(istream);
    value.v28 = F_unionHelper.read(istream);
    value.v29 = F_unionHelper.read(istream);
    return value;
  }

  public static void write(org.omg.CORBA.portable.OutputStream ostream,
                           F_except1 value
                          )
  {
    // write the repository ID
    ostream.write_string(id());
    F_structHelper.write(ostream, value.v1);
    F_unionHelper.write(ostream, value.v2);
    F_unionHelper.write(ostream, value.v3);
    F_unionHelper.write(ostream, value.v4);
    F_unionHelper.write(ostream, value.v5);
    F_unionHelper.write(ostream, value.v6);
    F_unionHelper.write(ostream, value.v7);
    F_unionHelper.write(ostream, value.v8);
    F_unionHelper.write(ostream, value.v9);
    F_unionHelper.write(ostream, value.v10);
    F_unionHelper.write(ostream, value.v11);
    F_unionHelper.write(ostream, value.v12);
    F_unionHelper.write(ostream, value.v13);
    F_unionHelper.write(ostream, value.v14);
    F_unionHelper.write(ostream, value.v15);
    F_unionHelper.write(ostream, value.v18);
    F_unionHelper.write(ostream, value.v19);
    F_unionHelper.write(ostream, value.v20);
    F_unionHelper.write(ostream, value.v21);
    F_unionHelper.write(ostream, value.v22);
    F_unionHelper.write(ostream, value.v23);
    F_unionHelper.write(ostream, value.v24);
    F_unionHelper.write(ostream, value.v25);
    F_unionHelper.write(ostream, value.v26);
    F_unionHelper.write(ostream, value.v27);
    F_unionHelper.write(ostream, value.v28);
    F_unionHelper.write(ostream, value.v29);
  }
}
