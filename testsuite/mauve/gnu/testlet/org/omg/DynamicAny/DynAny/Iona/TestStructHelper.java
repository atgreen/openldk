// Tags: not-a-test
// Copyright (c) IONA Technologies, 2001.

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

package gnu.testlet.org.omg.DynamicAny.DynAny.Iona;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.ObjectHelper;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

public abstract class TestStructHelper
{
  private static String _id =
    "IDL:gnu/testlet/org/omg/DynamicAny/DynAny/Iona/TestStruct:1.0";

  public static void insert(Any a, TestStruct that)
  {
    OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  public static TestStruct extract(Any a)
  {
    return read(a.create_input_stream());
  }

  private static TypeCode typeCode = null;

  public static TypeCode type()
  {
    if (typeCode == null)
      {
        StructMember[] members = new StructMember[ 17 ];

        ORB orb = ORB.init();

        TypeCode t_member;
        t_member = orb.get_primitive_tc(TCKind.tk_short);
        members [ 0 ] = new StructMember("shortVal", t_member, null);
        t_member = orb.get_primitive_tc(TCKind.tk_ushort);
        members [ 1 ] = new StructMember("ushortVal", t_member, null);
        t_member = orb.get_primitive_tc(TCKind.tk_long);
        members [ 2 ] = new StructMember("longVal", t_member, null);
        t_member = orb.get_primitive_tc(TCKind.tk_ulong);
        members [ 3 ] = new StructMember("ulongVal", t_member, null);
        t_member = orb.get_primitive_tc(TCKind.tk_float);
        members [ 4 ] = new StructMember("floatVal", t_member, null);
        t_member = orb.get_primitive_tc(TCKind.tk_double);
        members [ 5 ] = new StructMember("doubleVal", t_member, null);
        t_member = orb.get_primitive_tc(TCKind.tk_boolean);
        members [ 6 ] = new StructMember("boolVal", t_member, null);
        t_member = orb.get_primitive_tc(TCKind.tk_char);
        members [ 7 ] = new StructMember("charVal", t_member, null);
        t_member = orb.get_primitive_tc(TCKind.tk_octet);
        members [ 8 ] = new StructMember("octetVal", t_member, null);
        t_member = orb.get_primitive_tc(TCKind.tk_any);
        members [ 9 ] = new StructMember("anyVal", t_member, null);
        t_member = orb.get_primitive_tc(TCKind.tk_TypeCode);
        members [ 10 ] = new StructMember("tcVal", t_member, null);
        t_member = ObjectHelper.type();
        members [ 11 ] = new StructMember("objectVal", t_member, null);
        t_member = orb.create_string_tc(0);
        members [ 12 ] = new StructMember("stringVal", t_member, null);
        t_member = orb.get_primitive_tc(TCKind.tk_longlong);
        members [ 13 ] = new StructMember("longlongVal", t_member, null);
        t_member = orb.get_primitive_tc(TCKind.tk_ulonglong);
        members [ 14 ] = new StructMember("ulonglongVal", t_member, null);
        t_member = orb.get_primitive_tc(TCKind.tk_wchar);
        members [ 15 ] = new StructMember("wcharVal", t_member, null);
        t_member = orb.create_wstring_tc(0);
        members [ 16 ] = new StructMember("wstringVal", t_member, null);
        typeCode =
          orb.create_struct_tc(TestStructHelper.id(), "TestStruct", members);
      }
    return typeCode;
  }

  public static String id()
  {
    return _id;
  }

  public static TestStruct read(InputStream istream)
  {
    TestStruct value = new TestStruct();
    value.shortVal = istream.read_short();
    value.ushortVal = istream.read_ushort();
    value.longVal = istream.read_long();
    value.ulongVal = istream.read_ulong();
    value.floatVal = istream.read_float();
    value.doubleVal = istream.read_double();
    value.boolVal = istream.read_boolean();
    value.charVal = istream.read_char();
    value.octetVal = istream.read_octet();
    value.anyVal = istream.read_any();
    value.tcVal = istream.read_TypeCode();
    value.objectVal = ObjectHelper.read(istream);
    value.stringVal = istream.read_string();
    value.longlongVal = istream.read_longlong();
    value.ulonglongVal = istream.read_ulonglong();
    value.wcharVal = istream.read_wchar();
    value.wstringVal = istream.read_wstring();
    return value;
  }

  public static void write(OutputStream ostream, TestStruct value)
  {
    ostream.write_short(value.shortVal);
    ostream.write_ushort(value.ushortVal);
    ostream.write_long(value.longVal);
    ostream.write_ulong(value.ulongVal);
    ostream.write_float(value.floatVal);
    ostream.write_double(value.doubleVal);
    ostream.write_boolean(value.boolVal);
    ostream.write_char(value.charVal);
    ostream.write_octet(value.octetVal);
    ostream.write_any(value.anyVal);
    ostream.write_TypeCode(value.tcVal);
    ObjectHelper.write(ostream, value.objectVal);
    ostream.write_string(value.stringVal);
    ostream.write_longlong(value.longlongVal);
    ostream.write_ulonglong(value.ulonglongVal);
    ostream.write_wchar(value.wcharVal);
    ostream.write_wstring(value.wstringVal);
  }
}
