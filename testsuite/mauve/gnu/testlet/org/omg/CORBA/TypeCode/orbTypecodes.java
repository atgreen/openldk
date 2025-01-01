// Tags: JDK1.2

// Copyright (C) 2005 Audrius Meskauskas (AudriusA@Bioinformatics.org)

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.org.omg.CORBA.TypeCode;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.Asserter;

import org.omg.CORBA.ORB;
import org.omg.CORBA.ShortHolder;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.UnionMember;
import org.omg.CORBA.ValueMember;

/**
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public class orbTypecodes
  extends Asserter
  implements Testlet
{
  ORB orb = ORB.init();
  String id = "gnu";
  String name = "classpath";
  TypeCode type = new ShortHolder()._type();
  int length = 17;

  public void test(TestHarness harness)
  {
    h = harness;
    test_create_alias_tc();
    test_create_array_tc();
    test_create_enum_tc();
    test_create_exception_tc();
    test_create_fixed_tc();
    test_create_sequence_tc();
    test_create_struct_tc();
    test_create_union_tc();
    test_create_value_box_tc();
    test_create_value_tc();
    test_create_wstring_tc();
    test_get_primitive_tc();
  }

  public void test_create_alias_tc()
  {
    TypeCode t = orb.create_alias_tc(id, name, type);
    assertEquals("create_alias_tc", t.kind().value(), TCKind._tk_alias);
    idname(t);
  }

  public void test_create_array_tc()
  {
    TypeCode t = orb.create_array_tc(length, type);
    assertEquals("create_array_tc", t.kind().value(), TCKind._tk_array);
    length(t);
    component(t);
  }

  public void test_create_enum_tc()
  {
    TypeCode t = orb.create_enum_tc(id, name, new String[ 0 ]);
    assertEquals("create_enum_tc", t.kind().value(), TCKind._tk_enum);
    idname(t);
  }

  public void test_create_exception_tc()
  {
    TypeCode t = orb.create_exception_tc(id, name, new StructMember[ 0 ]);
    assertEquals("create_exception_tc", t.kind().value(), TCKind._tk_except);
    idname(t);
  }

  public void test_create_fixed_tc()
  {
    try
      {
        TypeCode t = orb.create_fixed_tc((short) 15, (short) 6);
        assertEquals("fixed digits", t.fixed_digits(), (short) 15);
        assertEquals("fixed scale", t.fixed_scale(), (short) 6);
        assertEquals("create_fixed_tc", t.kind().value(), TCKind._tk_fixed);
      }
    catch (BadKind ex)
      {
        fail("BadKind exception when testing fixed tc");
      }
  }

  public void test_create_sequence_tc()
  {
    TypeCode t = orb.create_sequence_tc(length, type);
    assertEquals("create_sequence_tc", t.kind().value(), TCKind._tk_sequence);
    length(t);
    component(t);
  }

  public void test_create_string_tc()
  {
    TypeCode t = orb.create_string_tc(length);
    assertEquals("create_string_tc", t.kind().value(), TCKind._tk_string);
    length(t);
  }

  public void test_create_struct_tc()
  {
    TypeCode t = orb.create_struct_tc(id, name, new StructMember[ 0 ]);
    assertEquals("create_struct_tc", t.kind().value(), TCKind._tk_struct);
    idname(t);
  }

  public void test_create_union_tc()
  {
    TypeCode t = orb.create_union_tc(id, name, type, new UnionMember[ 0 ]);
    assertEquals("create_union_tc", t.kind().value(), TCKind._tk_union);
    idname(t);
  }

  public void test_create_value_box_tc()
  {
    TypeCode t = orb.create_value_box_tc(id, name, type);
    assertEquals("create_value_box_tc", t.kind().value(), TCKind._tk_value_box);
    idname(t);
  }

  public void test_create_value_tc()
  {
    TypeCode t =
      orb.create_value_tc(id, name, (short) 0, type, new ValueMember[ 0 ]);
    assertEquals("create_value_tc", t.kind().value(), TCKind._tk_value);
    idname(t);
  }

  public void test_create_wstring_tc()
  {
    TypeCode t = orb.create_wstring_tc(length);
    assertEquals("create_wstring_tc", t.kind().value(), TCKind._tk_wstring);
    length(t);
  }

  public void test_get_primitive_tc()
  {
    TypeCode t = orb.get_primitive_tc(TCKind.tk_short);
    assertEquals("get_primitive_tc", t.kind().value(), TCKind._tk_short);
  }

  void component(TypeCode t)
  {
    try
      {
        assertEquals("sequence or array component type",
                     t.content_type().kind().value(), type.kind().value()
                    );
      }
    catch (BadKind ex)
      {
        fail("Unexpected bad kind for content type");
      }
  }

  void idname(TypeCode t)
  {
    try
      {
        assertEquals("id", t.id(), id);
        assertEquals("name", t.name(), name);
      }
    catch (BadKind ex)
      {
        fail("Unexpected bad kind for id or name");
      }
  }

  void length(TypeCode t)
  {
    try
      {
        assertEquals("content type", t.length(), length);
      }
    catch (BadKind ex)
      {
        fail("Unexpected bad kind for length");
      }
  }
}
