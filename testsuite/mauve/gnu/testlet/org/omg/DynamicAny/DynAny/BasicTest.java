// Tags: JDK1.4
// Uses: ../../PortableServer/POA/TestBase

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


// **********************************************************************
//
// Copyright (c) 2001
// IONA Technologies, Inc.
// Waltham, MA, USA
//
// All Rights Reserved
//
// **********************************************************************


package gnu.testlet.org.omg.DynamicAny.DynAny;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.DynamicAny.DynAny.Iona.TestEnum;
import gnu.testlet.org.omg.DynamicAny.DynAny.Iona.TestEnumHelper;
import gnu.testlet.org.omg.DynamicAny.DynAny.Iona.TestStruct;
import gnu.testlet.org.omg.DynamicAny.DynAny.Iona.TestStructHelper;
import gnu.testlet.org.omg.PortableServer.POA.TestBase;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyFactory;
import org.omg.DynamicAny.DynAnyFactoryHelper;
import org.omg.DynamicAny.DynEnum;
import org.omg.DynamicAny.DynEnumHelper;
import org.omg.DynamicAny.DynFixed;
import org.omg.DynamicAny.DynFixedHelper;
import org.omg.DynamicAny.DynStruct;
import org.omg.DynamicAny.DynStructHelper;
import org.omg.DynamicAny.NameDynAnyPair;
import org.omg.DynamicAny.NameValuePair;

import java.math.BigDecimal;

public class BasicTest
  extends TestBase
  implements Testlet
{
  final String ANY_VALUE = "This is a string in an any";
  final String STRING_VALUE = "This is a string";
  final String WSTRING_VALUE = "This is a wstring";
  final boolean BOOLEAN_VALUE = true;
  final byte OCTET_VALUE = (byte) 155;
  final char CHAR_VALUE = 'Y';
  final double DOUBLE_VALUE = 7.31e29;
  final float FLOAT_VALUE = (float) 1.9183;
  final int LONG_VALUE = -300000;
  final int ULONG_VALUE = 500000;
  final short SHORT_VALUE = (short) -10000;
  final short USHORT_VALUE = (short) 40000;

  //
  // Can't do this, because it causes a failure under JDK 1.2.2.
  // The problem is that ORB.init() is called before main() has
  // a chance to set the ORB properties, so the JDK ORB's
  // singleton implementation is used instead. This will result
  // in a NullPointerException due to a bug in that ORB.
  //
  // final TypeCode TYPECODE_VALUE =
  // ORB.init().get_primitive_tc(TCKind.tk_float);
  TypeCode TYPECODE_VALUE;
  final char WCHAR_VALUE = 'Z';
  final long LONGLONG_VALUE = -1234567890L;
  final long ULONGLONG_VALUE = 9876543210L;
  DynAnyFactory factory;
  ORB orb;

  public void setUp()
  {
    orb = org.omg.CORBA.ORB.init(new String[ 0 ], null);
    TYPECODE_VALUE = orb.get_primitive_tc(TCKind.tk_float);

    org.omg.CORBA.Object obj = null;

    try
      {
        obj = orb.resolve_initial_references("DynAnyFactory");
        TEST(obj != null);
        factory = DynAnyFactoryHelper.narrow(obj);
      }
    catch (org.omg.CORBA.ORBPackage.InvalidName ex)
      {
        TEST(false, ex.getMessage());
      }
  }

  public void allTests(ORB orb, Object o)
  {
    DynAnyFactory factory = DynAnyFactoryHelper.narrow(o);

    testBasic();
    testFixed();
    testEnum();
    testStruct();
  }

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    setUp();
    allTests(orb, factory);
    tearDown();
  }

  public void testBasic()
  {
    try
      {
        org.omg.CORBA.Object obj;
        Any any = orb.create_any();
        Any av;
        DynAny d1 = null;
        DynAny d2 = null;
        DynAny copy = null;
        TypeCode type;
        TypeCode tc;

        //
        // Test: short
        //
        type = orb.get_primitive_tc(TCKind.tk_short);
        d1 = factory.create_dyn_any_from_type_code(type);
        TEST(d1.get_short() == (short) 0);
        d1.insert_short((short) -53);
        TEST(d1.get_short() == (short) -53);
        d1.insert_short((short) 32000);
        TEST(d1.get_short() == (short) 32000);

        av = d1.to_any();

        short shortVal = av.extract_short();
        TEST(shortVal == (short) 32000);

        any.insert_short((short) 32000);
        d2 = factory.create_dyn_any(any);
        TEST(d1.equal(d2));

        copy = d1.copy();
        TEST(d1.equal(copy));
        TEST(copy.get_short() == (short) 32000);

        any.insert_short((short) -99);
        d2.from_any(any);
        d1.assign(d2);
        TEST(d1.equal(d2));

        d1.destroy();
        d2.destroy();
        copy.destroy();

        testOps(orb, factory, type, false);

        //
        // Test: unsigned short
        //
        type = orb.get_primitive_tc(TCKind.tk_ushort);
        d1 = factory.create_dyn_any_from_type_code(type);
        TEST(d1.get_ushort() == (short) 0);
        d1.insert_ushort((short) 199);
        TEST(d1.get_ushort() == (short) 199);
        d1.insert_ushort((short) 65001);
        TEST(d1.get_ushort() == (short) 65001);

        av = d1.to_any();

        short ushortVal = av.extract_ushort();
        TEST(ushortVal == (short) 65001);

        any.insert_ushort((short) 65001);
        d2 = factory.create_dyn_any(any);
        TEST(d1.equal(d2));

        copy = d1.copy();
        TEST(d1.equal(copy));
        TEST(copy.get_ushort() == (short) 65001);

        any.insert_ushort((short) 501);
        d2.from_any(any);
        d1.assign(d2);
        TEST(d1.equal(d2));

        d1.destroy();
        d2.destroy();
        copy.destroy();

        testOps(orb, factory, type, false);

        //
        // Test: long
        //
        type = orb.get_primitive_tc(TCKind.tk_long);
        d1 = factory.create_dyn_any_from_type_code(type);
        TEST(d1.get_long() == 0);
        d1.insert_long(-530000);
        TEST(d1.get_long() == -530000);
        d1.insert_long(3200000);
        TEST(d1.get_long() == 3200000);

        av = d1.to_any();

        int longVal = av.extract_long();
        TEST(longVal == 3200000);

        any.insert_long(3200000);
        d2 = factory.create_dyn_any(any);
        TEST(d1.equal(d2));

        copy = d1.copy();
        TEST(d1.equal(copy));
        TEST(copy.get_long() == 3200000);

        any.insert_long(-99000);
        d2.from_any(any);
        d1.assign(d2);
        TEST(d1.equal(d2));

        d1.destroy();
        d2.destroy();
        copy.destroy();

        testOps(orb, factory, type, false);

        //
        // Test: unsigned long
        //
        type = orb.get_primitive_tc(TCKind.tk_ulong);
        d1 = factory.create_dyn_any_from_type_code(type);
        TEST(d1.get_ulong() == 0);
        d1.insert_ulong(199000);
        TEST(d1.get_ulong() == 199000);
        d1.insert_ulong(65001000);
        TEST(d1.get_ulong() == 65001000);

        av = d1.to_any();

        int ulongVal = av.extract_ulong();
        TEST(ulongVal == 65001000);

        any.insert_ulong(65001000);
        d2 = factory.create_dyn_any(any);
        TEST(d1.equal(d2));

        copy = d1.copy();
        TEST(d1.equal(copy));
        TEST(copy.get_ulong() == 65001000);

        any.insert_ulong(501000);
        d2.from_any(any);
        d1.assign(d2);
        TEST(d1.equal(d2));

        d1.destroy();
        d2.destroy();
        copy.destroy();

        testOps(orb, factory, type, false);

        //
        // Test: float
        //
        type = orb.get_primitive_tc(TCKind.tk_float);
        d1 = factory.create_dyn_any_from_type_code(type);
        TEST(d1.get_float() == 0.0f);
        d1.insert_float(199.001f);
        TEST(d1.get_float() > 199.0f && d1.get_float() < 199.1f);
        d1.insert_float(6500.10001f);
        TEST(d1.get_float() > 6500.0f && d1.get_float() < 6501.0f);

        av = d1.to_any();

        float floatVal = av.extract_float();
        TEST(floatVal > 6500.1 && floatVal < 6500.2);

        any.insert_float((float) 6500.10001);
        d2 = factory.create_dyn_any(any);
        TEST(d1.equal(d2));

        copy = d1.copy();
        TEST(d1.equal(copy));
        TEST(copy.get_float() > 6500.1 && copy.get_float() < 6500.2);

        any.insert_float((float) 501.001);
        d2.from_any(any);
        d1.assign(d2);
        TEST(d1.equal(d2));

        d1.destroy();
        d2.destroy();
        copy.destroy();

        testOps(orb, factory, type, false);

        //
        // Test: double
        //
        type = orb.get_primitive_tc(TCKind.tk_double);
        d1 = factory.create_dyn_any_from_type_code(type);
        TEST(d1.get_double() == 0.0);
        d1.insert_double(199000.001);
        TEST(d1.get_double() > 199000.0 && d1.get_double() < 199000.1);
        d1.insert_double(6500000.10001);
        TEST(d1.get_double() > 6500000.1 && d1.get_double() < 6500000.2);

        av = d1.to_any();

        double doubleVal = av.extract_double();
        TEST(doubleVal > 6500000.1 && doubleVal < 6500000.2);

        any.insert_double(6500000.10001);
        d2 = factory.create_dyn_any(any);
        TEST(d1.equal(d2));

        copy = d1.copy();
        TEST(d1.equal(copy));
        TEST(copy.get_double() > 6500000.1 && copy.get_double() < 6500000.2);

        any.insert_double(501000.001);
        d2.from_any(any);
        d1.assign(d2);
        TEST(d1.equal(d2));

        d1.destroy();
        d2.destroy();
        copy.destroy();

        testOps(orb, factory, type, false);

        //
        // Test: boolean
        //
        type = orb.get_primitive_tc(TCKind.tk_boolean);
        d1 = factory.create_dyn_any_from_type_code(type);
        TEST(d1.get_boolean() == false);
        d1.insert_boolean(false);
        TEST(d1.get_boolean() == false);
        d1.insert_boolean(true);
        TEST(d1.get_boolean() == true);

        av = d1.to_any();

        boolean boolVal = av.extract_boolean();
        TEST(boolVal == true);

        any.insert_boolean(true);
        d2 = factory.create_dyn_any(any);
        TEST(d1.equal(d2));

        copy = d1.copy();
        TEST(d1.equal(copy));
        TEST(copy.get_boolean() == true);

        any.insert_boolean(false);
        d2.from_any(any);
        d1.assign(d2);
        TEST(d1.equal(d2));

        d1.destroy();
        d2.destroy();
        copy.destroy();

        testOps(orb, factory, type, false);

        //
        // Test: char
        //
        type = orb.get_primitive_tc(TCKind.tk_char);
        d1 = factory.create_dyn_any_from_type_code(type);
        TEST(d1.get_char() == 0);
        d1.insert_char('A');
        TEST(d1.get_char() == 'A');
        d1.insert_char('z');
        TEST(d1.get_char() == 'z');

        av = d1.to_any();

        char charVal = av.extract_char();
        TEST(charVal == 'z');

        any.insert_char('z');
        d2 = factory.create_dyn_any(any);
        TEST(d1.equal(d2));

        copy = d1.copy();
        TEST(d1.equal(copy));
        TEST(copy.get_char() == 'z');

        any.insert_char('@');
        d2.from_any(any);
        d1.assign(d2);
        TEST(d1.equal(d2));

        d1.destroy();
        d2.destroy();
        copy.destroy();

        testOps(orb, factory, type, false);

        //
        // Test: octet
        //
        type = orb.get_primitive_tc(TCKind.tk_octet);
        d1 = factory.create_dyn_any_from_type_code(type);
        TEST(d1.get_octet() == 0);
        d1.insert_octet((byte) 255);
        TEST(d1.get_octet() == (byte) 255);
        d1.insert_octet((byte) 1);
        TEST(d1.get_octet() == (byte) 1);

        av = d1.to_any();

        byte octetVal = av.extract_octet();
        TEST(octetVal == (byte) 1);

        any.insert_octet((byte) 1);
        d2 = factory.create_dyn_any(any);
        TEST(d1.equal(d2));

        copy = d1.copy();
        TEST(d1.equal(copy));
        TEST(copy.get_octet() == (byte) 1);

        any.insert_octet((byte) 127);
        d2.from_any(any);
        d1.assign(d2);
        TEST(d1.equal(d2));

        d1.destroy();
        d2.destroy();
        copy.destroy();

        testOps(orb, factory, type, false);

        //
        // Test: any
        //
        type = orb.get_primitive_tc(TCKind.tk_any);
        d1 = factory.create_dyn_any_from_type_code(type);
        any.insert_long(345678);
        d1.insert_any(any);
        av = d1.get_any();
        longVal = av.extract_long();
        TEST(longVal == 345678);

        any = orb.create_any();

        Any anyVal = orb.create_any();
        anyVal.insert_long(345678);
        any.insert_any(anyVal);
        d2 = factory.create_dyn_any(any);
        TEST(d1.equal(d2));

        av = d1.to_any();

        Any cap = av.extract_any();
        longVal = cap.extract_long();
        TEST(longVal == 345678);

        anyVal.insert_string("anyValue");
        any.insert_any(anyVal);
        d2.from_any(any);
        d1.assign(d2);
        TEST(d1.equal(d2));

        copy = d1.copy();
        TEST(d1.equal(copy));

        d1.destroy();
        d2.destroy();
        copy.destroy();

        testOps(orb, factory, type, false);
      }
    catch (org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode ex)
      {
        TEST(false);
      }
    catch (org.omg.DynamicAny.DynAnyPackage.TypeMismatch ex)
      {
        ex.printStackTrace();
        TEST(false, ex.getMessage());
      }
    catch (org.omg.DynamicAny.DynAnyPackage.InvalidValue ex)
      {
        TEST(false);
      }
  }

  public void testEnum()
  {
    try
      {
        Any any = orb.create_any();
        Any av;
        DynAny d1 = null;
        DynAny d2 = null;
        DynAny copy = null;
        String str;
        DynEnum e1;
        DynEnum e2;
        TestEnum e;
        TypeCode type = TestEnumHelper.type();

        //
        // Test: initial value
        //
        d1 = factory.create_dyn_any_from_type_code(type);
        e1 = DynEnumHelper.narrow(d1);
        TEST(e1.get_as_ulong() == 0);
        str = e1.get_as_string();
        TEST(str.equals("red"));

        //
        // Test: set_as_string()
        //
        e1.set_as_string("green");
        TEST(e1.get_as_ulong() == 1);
        str = e1.get_as_string();
        TEST(str.equals("green"));
        e1.set_as_string("blue");
        TEST(e1.get_as_ulong() == 2);
        str = e1.get_as_string();
        TEST(str.equals("blue"));

        //
        // Test: set_as_ulong()
        //
        e1.set_as_ulong(1);
        TEST(e1.get_as_ulong() == 1);
        str = e1.get_as_string();
        TEST(str.equals("green"));
        e1.set_as_ulong(2);
        TEST(e1.get_as_ulong() == 2);
        str = e1.get_as_string();
        TEST(str.equals("blue"));

        //
        // Test: from_any()
        //
        TestEnumHelper.insert(any, TestEnum.green);
        e1.from_any(any);

        //
        // Test: to_any()
        //
        av = e1.to_any();
        e = TestEnumHelper.extract(av);
        TEST(e == TestEnum.green);

        //
        // Test: copy
        //
        copy = e1.copy();
        TEST(e1.equal(copy));

        e1.destroy();
        copy.destroy();

        //
        // Test: set_as_ulong() InvalidValue exception
        //
        try
          {
            d1 = factory.create_dyn_any_from_type_code(type);
            e1 = DynEnumHelper.narrow(d1);
            e1.set_as_ulong(3);
            TEST("set_as_ulong() should not have succeeded" == null);
          }
        catch (org.omg.DynamicAny.DynAnyPackage.InvalidValue ex)
          {
            // expected
            d1.destroy();
          }
        try
          {
            d1 = factory.create_dyn_any_from_type_code(type);
            e1 = DynEnumHelper.narrow(d1);

            //
            // In Java there is no *unsigned* int, so we need an
            // additional test case not required for C++.
            //
            e1.set_as_ulong(-1);
            TEST("set_as_ulong() should not have succeeded" == null);
          }
        catch (org.omg.DynamicAny.DynAnyPackage.InvalidValue ex)
          {
            // expected
            d1.destroy();
          }

        //
        // Test: set_as_string() InvalidValue exception
        //
        try
          {
            d1 = factory.create_dyn_any_from_type_code(type);
            e1 = DynEnumHelper.narrow(d1);
            e1.set_as_string("alizarin");
            TEST("set_as_string() should not have succeeded" == null);
          }
        catch (org.omg.DynamicAny.DynAnyPackage.InvalidValue ex)
          {
            // expected
            d1.destroy();
          }

        testOps(orb, factory, TestEnumHelper.type(), false);
      }
    catch (org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode ex)
      {
        TEST(false);
      }
    catch (org.omg.DynamicAny.DynAnyPackage.TypeMismatch ex)
      {
        TEST(false);
      }
    catch (org.omg.DynamicAny.DynAnyPackage.InvalidValue ex)
      {
        TEST(false);
      }
  }

  public void testFixed()
  {
    try
      {
        Any any = orb.create_any();
        Any av;
        DynAny d1 = null;
        DynAny d2 = null;
        DynAny copy = null;
        TypeCode tc;
        String str;
        DynFixed f1;
        DynFixed f2;
        BigDecimal f;

        //
        // Create TypeCode
        //
        tc = orb.create_fixed_tc((short) 5, (short) 2);

        //
        // Test: get_value()
        //
        d1 = factory.create_dyn_any_from_type_code(tc);
        f1 = DynFixedHelper.narrow(d1);
        str = f1.get_value();
        f = new BigDecimal(str);
        TEST(Math.abs(f.floatValue() - 0.0) < Float.MIN_VALUE);

        //
        // Test: set_value()
        //
        TEST(f1.set_value("1.1"));
        TEST(f1.set_value("123.1"));
        TEST(f1.set_value("123.12"));
        TEST(!f1.set_value("123.123"));

        //
        // Test: from_any()
        //
        f = new BigDecimal("98");
        any.insert_fixed(f, tc);
        f1.from_any(any);

        //
        // Test: to_any()
        //
        av = f1.to_any();
        f = av.extract_fixed();
        TEST(f.equals(new BigDecimal("98")));

        //
        // Test: copy
        //
        copy = f1.copy();
        TEST(f1.equal(copy));

        f1.destroy();
        copy.destroy();

        //
        // Test: set_value() InvalidValue exception (part 1)
        //
        try
          {
            d1 = factory.create_dyn_any_from_type_code(tc);
            f1 = DynFixedHelper.narrow(d1);
            f1.set_value("");
            harness.fail("set_value() should not have succeeded");
          }
        catch (Exception ex)
          {
            // expected
            d1.destroy();
          }

        //
        // Test: assign() TypeMismatch exception
        //
        try
          {
            f = new BigDecimal("99");
            any.insert_fixed(f, orb.create_fixed_tc((short) 4, (short) 2));
            d1 = factory.create_dyn_any(any);
            d2 = factory.create_dyn_any_from_type_code(tc);
            d2.assign(d1);
            harness.fail("assign() should not have succeeded");
          }
        catch (org.omg.DynamicAny.DynAnyPackage.TypeMismatch ex)
          {
            // expected
            d1.destroy();
            d2.destroy();
          }

        //
        // Test: from_any() TypeMismatch exception
        //
        try
          {
            f = new BigDecimal("99");
            any.insert_fixed(f, orb.create_fixed_tc((short) 4, (short) 2));
            d1 = factory.create_dyn_any_from_type_code(tc);
            d1.from_any(any);
            harness.fail("from_any() should not have succeeded");
          }
        catch (org.omg.DynamicAny.DynAnyPackage.TypeMismatch ex)
          {
            // expected
            d1.destroy();
          }

        testOps(orb, factory, tc, false);
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        fail(ex);
      }
  }

  public void testStruct()
  {
    try
      {
        int i;
        Any any = orb.create_any();
        Any av;
        DynAny d1;
        DynAny d2;
        DynAny copy;
        String str;
        String wstr;
        DynStruct s1;
        DynStruct s2;
        String cp;
        TypeCode type;
        TypeCode tc;
        TestStruct ts = new TestStruct();
        TestStruct pts;
        NameValuePair[] nvpseq;
        NameDynAnyPair[] ndpseq;

        type = TestStructHelper.type();
        d1 = factory.create_dyn_any_from_type_code(type);
        s1 = DynStructHelper.narrow(d1);

        //
        // Test: current_member_name, current_member_kind
        //
        str = s1.current_member_name();
        TEST(str.equals("shortVal"));
        TEST(s1.current_member_kind() == TCKind.tk_short);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("ushortVal"));
        TEST(s1.current_member_kind() == TCKind.tk_ushort);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("longVal"));
        TEST(s1.current_member_kind() == TCKind.tk_long);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("ulongVal"));
        TEST(s1.current_member_kind() == TCKind.tk_ulong);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("floatVal"));
        TEST(s1.current_member_kind() == TCKind.tk_float);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("doubleVal"));
        TEST(s1.current_member_kind() == TCKind.tk_double);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("boolVal"));
        TEST(s1.current_member_kind() == TCKind.tk_boolean);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("charVal"));
        TEST(s1.current_member_kind() == TCKind.tk_char);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("octetVal"));
        TEST(s1.current_member_kind() == TCKind.tk_octet);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("anyVal"));
        TEST(s1.current_member_kind() == TCKind.tk_any);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("tcVal"));
        TEST(s1.current_member_kind() == TCKind.tk_TypeCode);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("objectVal"));
        TEST(s1.current_member_kind() == TCKind.tk_objref);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("stringVal"));
        TEST(s1.current_member_kind() == TCKind.tk_string);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("longlongVal"));
        TEST(s1.current_member_kind() == TCKind.tk_longlong);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("ulonglongVal"));
        TEST(s1.current_member_kind() == TCKind.tk_ulonglong);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("wcharVal"));
        TEST(s1.current_member_kind() == TCKind.tk_wchar);
        s1.next();
        str = s1.current_member_name();
        TEST(str.equals("wstringVal"));
        TEST(s1.current_member_kind() == TCKind.tk_wstring);

        //
        // Test: insert values into members
        //
        s1.rewind();
        s1.insert_short(SHORT_VALUE);
        s1.next();
        s1.insert_ushort(USHORT_VALUE);
        s1.next();
        s1.insert_long(LONG_VALUE);
        s1.next();
        s1.insert_ulong(ULONG_VALUE);
        s1.next();
        s1.insert_float(FLOAT_VALUE);
        s1.next();
        s1.insert_double(DOUBLE_VALUE);
        s1.next();
        s1.insert_boolean(BOOLEAN_VALUE);
        s1.next();
        s1.insert_char(CHAR_VALUE);
        s1.next();
        s1.insert_octet(OCTET_VALUE);
        s1.next();

        DynAny d1c = d1.current_component();

        any.insert_string(ANY_VALUE);
        s1.insert_any(any);
        s1.next();
        s1.insert_typecode(TYPECODE_VALUE);
        s1.next();
        s1.insert_reference(null);
        s1.next();
        s1.insert_string(STRING_VALUE);
        s1.next();
        s1.insert_longlong(LONGLONG_VALUE);
        s1.next();
        s1.insert_ulonglong(ULONGLONG_VALUE);
        s1.next();
        s1.insert_wchar(WCHAR_VALUE);
        s1.next();
        s1.insert_wstring(WSTRING_VALUE);
        s1.next();

        //
        // Test: get values from members
        //
        s1.rewind();
        TEST(s1.get_short() == SHORT_VALUE);
        s1.next();
        TEST(s1.get_ushort() == USHORT_VALUE);
        s1.next();
        TEST(s1.get_long() == LONG_VALUE);
        s1.next();
        TEST(s1.get_ulong() == ULONG_VALUE);
        s1.next();
        TEST(s1.get_float() == FLOAT_VALUE);
        s1.next();
        TEST(s1.get_double() == DOUBLE_VALUE);
        s1.next();
        TEST(s1.get_boolean() == BOOLEAN_VALUE);
        s1.next();
        TEST(s1.get_char() == CHAR_VALUE);
        s1.next();
        TEST(s1.get_octet() == OCTET_VALUE);
        s1.next();
        av = s1.get_any();

        TEST(av.extract_string().equals(ANY_VALUE));
        s1.next();
        tc = s1.get_typecode();
        s1.next();
        TEST(tc.equal(TYPECODE_VALUE));
        TEST(s1.get_reference() == null);
        s1.next();
        str = s1.get_string();
        s1.next();
        TEST(str.equals(STRING_VALUE));
        TEST(s1.get_longlong() == LONGLONG_VALUE);
        s1.next();
        TEST(s1.get_ulonglong() == ULONGLONG_VALUE);
        s1.next();
        TEST(s1.get_wchar() == WCHAR_VALUE);
        s1.next();
        wstr = s1.get_wstring();
        s1.next();
        TEST(wstr.equals(WSTRING_VALUE));

        //
        // Test: get_members
        //
        nvpseq = s1.get_members();
        s1.rewind();
        for (i = 0; i < 11; i++)
          {
            str = s1.current_member_name();
            TEST(str.equals(nvpseq [ i ].id));

            DynAny dv = factory.create_dyn_any(nvpseq [ i ].value);
            DynAny comp = s1.current_component();
            TEST(dv.equal(comp));
            dv.destroy();
            s1.next();
          }

        //
        // Test: get_members_as_dyn_any
        //
        ndpseq = s1.get_members_as_dyn_any();
        s1.rewind();
        for (i = 0; i < 11; i++)
          {
            str = s1.current_member_name();
            TEST(str.equals(ndpseq [ i ].id));
            s1.next();
          }
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        fail(ex);
      }
  }

  protected void tearDown()
  {
    orb.destroy();
  }

  void checkStruct(final TestStruct ts)
  {
    TEST(ts.shortVal == SHORT_VALUE);
    TEST(ts.ushortVal == USHORT_VALUE);
    TEST(ts.longVal == LONG_VALUE);
    TEST(ts.ulongVal == ULONG_VALUE);
    TEST(ts.floatVal == FLOAT_VALUE);
    TEST(ts.doubleVal == DOUBLE_VALUE);
    TEST(ts.boolVal == BOOLEAN_VALUE);
    TEST(ts.charVal == CHAR_VALUE);
    TEST(ts.octetVal == OCTET_VALUE);
    TEST(ts.anyVal.extract_string().equals(ANY_VALUE));
    TEST(ts.tcVal.equal(TYPECODE_VALUE));
    TEST(ts.objectVal == null);
    TEST(ts.stringVal.equals(STRING_VALUE));
    TEST(ts.longlongVal == LONGLONG_VALUE);
    TEST(ts.ulonglongVal == ULONGLONG_VALUE);
    TEST(ts.wcharVal == WCHAR_VALUE);
    TEST(ts.wstringVal.equals(WSTRING_VALUE));
  }

  void loadStruct(ORB orb, TestStruct ts)
  {
    ts.shortVal = SHORT_VALUE;
    ts.ushortVal = USHORT_VALUE;
    ts.longVal = LONG_VALUE;
    ts.ulongVal = ULONG_VALUE;
    ts.floatVal = FLOAT_VALUE;
    ts.doubleVal = DOUBLE_VALUE;
    ts.boolVal = BOOLEAN_VALUE;
    ts.charVal = CHAR_VALUE;
    ts.octetVal = OCTET_VALUE;
    ts.anyVal = orb.create_any();
    ts.anyVal.insert_string(ANY_VALUE);
    ts.tcVal = TYPECODE_VALUE;
    ts.objectVal = null;
    ts.stringVal = STRING_VALUE;
    ts.longlongVal = LONGLONG_VALUE;
    ts.ulonglongVal = ULONGLONG_VALUE;
    ts.wcharVal = WCHAR_VALUE;
    ts.wstringVal = WSTRING_VALUE;
  }

  //
  // Test generic operations
  //
  void testOps(ORB orb, DynAnyFactory factory, TypeCode tc,
               boolean hasComponents
              )
  {
    try
      {
        Any badAny = orb.create_any();
        DynAny d1 = null;
        DynAny d2 = null;
        DynAny d3 = null;
        DynAny copy = null;
        TypeCode origTC = getOrigType(tc);

        //
        // Create an any having a TypeCode that will not match tc
        //
        if (tc.kind() != TCKind.tk_short)
          badAny.insert_short((short) 0);
        else
          badAny.insert_ushort((short) 0);

        //
        // Test: type()
        //
        d1 = factory.create_dyn_any_from_type_code(tc);

        TypeCode tcv = d1.type();
        TEST(tc.equal(tcv));
        d1.destroy();

        //
        // Test: assign() TypeMismatch exception
        //
        try
          {
            d1 = factory.create_dyn_any_from_type_code(tc);
            d2 = factory.create_dyn_any(badAny);
            d1.assign(d2);
            TEST("assign() should not have succeeded" == null);
          }
        catch (org.omg.DynamicAny.DynAnyPackage.TypeMismatch ex)
          {
            // expected
            d1.destroy();
            d2.destroy();
          }

        //
        // Test: from_any() TypeMismatch exception
        //
        try
          {
            d1 = factory.create_dyn_any_from_type_code(tc);
            d1.from_any(badAny);
            TEST("from_any() should not have succeeded" == null);
          }
        catch (org.omg.DynamicAny.DynAnyPackage.TypeMismatch ex)
          {
            // expected
            d1.destroy();
          }

        //
        // Test: from_any() InvalidValue exception
        //
        switch (origTC.kind().value())
          {
            case TCKind._tk_null :
            case TCKind._tk_void :
            case TCKind._tk_TypeCode :
            case TCKind._tk_Principal :
            case TCKind._tk_objref :
            case TCKind._tk_value :
            case TCKind._tk_value_box :

              // nothing to do
              break;

            default :
              try
                {
                  Any a = orb.create_any();
                  a.type(tc);
                  d1 = factory.create_dyn_any_from_type_code(tc);
                  d1.from_any(a);
                  harness.fail("from_any() should not have succeeded");
                }
              catch (org.omg.DynamicAny.DynAnyPackage.InvalidValue ex)
                {
                  // expected
                  d1.destroy();
                }
          }

        if (hasComponents)
          {
            int count;

            d1 = factory.create_dyn_any_from_type_code(tc);

            if (origTC.kind() == TCKind.tk_union)
              count = d1.component_count();
            else
              count = origTC.member_count();
            TEST(count > 0);

            //
            // Test: seek
            //
            TEST(d1.seek(0) == true);
            TEST(d1.seek(-1) == false);
            TEST(d1.seek(count) == false);
            TEST(d1.seek(count - 1) == true);

            //
            // Test: next
            //
            d1.seek(-1);
            TEST(d1.next() == true);
            d1.seek(count - 1);
            TEST(d1.next() == false);

            //
            // Test: component_count()
            //
            TEST(d1.component_count() == count);

            //
            // Test: current_component
            //
            d1.rewind();
            d2 = d1.current_component();
            TEST(d2 != null);

            //
            // Test: destroy
            //
            d2.destroy(); // should do nothing because it's a child
            d2.destroy(); // ditto

            //
            // Test: current_component
            //
            d1.seek(-9);
            d3 = d1.current_component();
            TEST(d3 == null);

            d1.destroy();
          }
        else
          {
            d1 = factory.create_dyn_any_from_type_code(tc);

            //
            // Test: seek
            //
            TEST(d1.seek(0) == false);
            TEST(d1.seek(-1) == false);

            //
            // Test: next
            //
            TEST(d1.next() == false);

            //
            // Test: component_count()
            //
            TEST(d1.component_count() == 0);
          }
      }
    catch (Exception ex)
      {
        fail(ex);
      }
  }
}