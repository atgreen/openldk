// Tags: JDK1.1

// Copyright (C) 2005 Jeroen Frijters

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

package gnu.testlet.java.lang.reflect.Field;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.lang.reflect.Field;

public class promotion implements Testlet
{
  public boolean booleanField = true;
  public byte byteField = (byte)42;
  public char charField = (char)42;
  public short shortField = (short)42;
  public int intField = 42;
  public float floatField = 42f;
  public long longField = 42L;
  public double doubleField = 42.0;
  public Integer intObjField;

  public void test(TestHarness harness)
  {
    Class c = promotion.class;
    try
      {
        testBooleanField(harness, c.getField("booleanField"));
        testByteField(harness, c.getField("byteField"));
        testCharField(harness, c.getField("charField"));
        testShortField(harness, c.getField("shortField"));
        testIntField(harness, c.getField("intField"));
        testFloatField(harness, c.getField("floatField"));
        testLongField(harness, c.getField("longField"));
        testDoubleField(harness, c.getField("doubleField"));

        try
          {
            c.getField("intObjField").getInt(this);
          }
        catch (IllegalArgumentException _)
          {
            harness.check(true);
          }
        try
          {
            c.getField("intObjField").setInt(this, 0);
          }
        catch (IllegalArgumentException _)
          {
            harness.check(true);
          }
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }
  }

  private void testBooleanField(TestHarness harness, Field field)
  {
    harness.checkPoint("boolean");
    testGetIllegalArgument(harness, field, new boolean[] {
        false, true, true, true, true, true, true, true });
    testSetIllegalArgument(harness, field, new boolean[] {
        false, true, true, true, true, true, true, true });
    try
      {
        harness.check(field.getBoolean(this) == booleanField);
        harness.check(field.get(this).equals(new Boolean(booleanField)));

        field.setBoolean(this, booleanField);
        harness.check(true);
        field.set(this, new Boolean(booleanField));
        harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }
  }

  private void testByteField(TestHarness harness, Field field)
  {
    harness.checkPoint("byte");
    testGetIllegalArgument(harness, field, new boolean[] {
        true, false, true, false, false, false, false, false });
    testSetIllegalArgument(harness, field, new boolean[] {
        true, false, true, true, true, true, true, true });
    try
      {
        harness.check(field.getByte(this) == byteField);
        harness.check(field.getShort(this) == byteField);
        harness.check(field.getInt(this) == byteField);
        harness.check(field.getFloat(this) == byteField);
        harness.check(field.getLong(this) == byteField);
        harness.check(field.getDouble(this) == byteField);
        harness.check(field.get(this).equals(new Byte(byteField)));

        field.setByte(this, byteField);
        harness.check(true);
        field.set(this, new Byte(byteField));
        harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }
  }

  private void testCharField(TestHarness harness, Field field)
  {
    harness.checkPoint("char");
    testGetIllegalArgument(harness, field, new boolean[] {
        true, true, false, true, false, false, false, false });
    testSetIllegalArgument(harness, field, new boolean[] {
        true, true, false, true, true, true, true, true });
    try
      {
        harness.check(field.getChar(this) == charField);
        harness.check(field.getInt(this) == charField);
        harness.check(field.getFloat(this) == charField);
        harness.check(field.getLong(this) == charField);
        harness.check(field.getDouble(this) == charField);
        harness.check(field.get(this).equals(new Character(charField)));

        field.setChar(this, charField);
        harness.check(true);
        field.set(this, new Character(charField));
        harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }
  }

  private void testShortField(TestHarness harness, Field field)
  {
    harness.checkPoint("short");
    testGetIllegalArgument(harness, field, new boolean[] {
        true, true, true, false, false, false, false, false });
    testSetIllegalArgument(harness, field, new boolean[] {
        true, false, true, false, true, true, true, true });
    try
      {
        harness.check(field.getShort(this) == shortField);
        harness.check(field.getInt(this) == shortField);
        harness.check(field.getFloat(this) == shortField);
        harness.check(field.getLong(this) == shortField);
        harness.check(field.getDouble(this) == shortField);
        harness.check(field.get(this).equals(new Short(shortField)));

        field.setByte(this, (byte)shortField);
        harness.check(true);
        field.setShort(this, shortField);
        harness.check(true);
        field.set(this, new Byte((byte)shortField));
        harness.check(true);
        field.set(this, new Short(shortField));
        harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }
  }

  private void testIntField(TestHarness harness, Field field)
  {
    harness.checkPoint("int");
    testGetIllegalArgument(harness, field, new boolean[] {
        true, true, true, true, false, false, false, false });
    testSetIllegalArgument(harness, field, new boolean[] {
        true, false, false, false, false, true, true, true });
    try
      {
        harness.check(field.getInt(this) == intField);
        harness.check(field.getFloat(this) == intField);
        harness.check(field.getLong(this) == intField);
        harness.check(field.getDouble(this) == intField);
        harness.check(field.get(this).equals(new Integer(intField)));

        field.setByte(this, (byte)intField);
        harness.check(true);
        field.setChar(this, (char)intField);
        harness.check(true);
        field.setShort(this, (short)intField);
        harness.check(true);
        field.setInt(this, intField);
        harness.check(true);
        field.set(this, new Byte((byte)intField));
        harness.check(true);
        field.set(this, new Character((char)intField));
        harness.check(true);
        field.set(this, new Short((short)intField));
        harness.check(true);
        field.set(this, new Integer(intField));
        harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }
  }

  private void testFloatField(TestHarness harness, Field field)
  {
    harness.checkPoint("float");
    testGetIllegalArgument(harness, field, new boolean[] {
        true, true, true, true, true, false, true, false });
    testSetIllegalArgument(harness, field, new boolean[] {
        true, false, false, false, false, false, false, true });
    try
      {
        harness.check(field.getFloat(this) == floatField);
        harness.check(field.getDouble(this) == floatField);
        harness.check(field.get(this).equals(new Float(floatField)));

        field.setByte(this, (byte)floatField);
        harness.check(true);
        field.setChar(this, (char)floatField);
        harness.check(true);
        field.setShort(this, (short)floatField);
        harness.check(true);
        field.setInt(this, (int)floatField);
        harness.check(true);
        field.setFloat(this, floatField);
        harness.check(true);
        field.setLong(this, (long)floatField);
        harness.check(true);
        field.set(this, new Byte((byte)floatField));
        harness.check(true);
        field.set(this, new Character((char)floatField));
        harness.check(true);
        field.set(this, new Short((short)floatField));
        harness.check(true);
        field.set(this, new Integer((int)floatField));
        harness.check(true);
        field.set(this, new Float(floatField));
        harness.check(true);
        field.set(this, new Long((long)floatField));
        harness.check(true);
    }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }
  }

  private void testLongField(TestHarness harness, Field field)
  {
    harness.checkPoint("long");
    testGetIllegalArgument(harness, field, new boolean[] {
        true, true, true, true, true, false, false, false });
    testSetIllegalArgument(harness, field, new boolean[] {
        true, false, false, false, false, true, false, true });
    try
      {
        harness.check(field.getFloat(this) == longField);
        harness.check(field.getLong(this) == longField);
        harness.check(field.getDouble(this) == longField);
        harness.check(field.get(this).equals(new Long(longField)));

        field.setByte(this, (byte)longField);
        harness.check(true);
        field.setChar(this, (char)longField);
        harness.check(true);
        field.setShort(this, (short)longField);
        harness.check(true);
        field.setInt(this, (int)longField);
        harness.check(true);
        field.setLong(this, longField);
        harness.check(true);
        field.set(this, new Byte((byte)longField));
        harness.check(true);
        field.set(this, new Character((char)longField));
        harness.check(true);
        field.set(this, new Short((short)longField));
        harness.check(true);
        field.set(this, new Integer((int)longField));
        harness.check(true);
        field.set(this, new Long(longField));
        harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }
  }

  private void testDoubleField(TestHarness harness, Field field)
  {
    harness.checkPoint("double");
    testGetIllegalArgument(harness, field, new boolean[] {
        true, true, true, true, true, true, true, false });
    testSetIllegalArgument(harness, field, new boolean[] {
        true, false, false, false, false, false, false, false });
    try
      {
        harness.check(field.getDouble(this) == doubleField);
        harness.check(field.get(this).equals(new Double(doubleField)));

        field.setByte(this, (byte)doubleField);
        harness.check(true);
        field.setChar(this, (char)doubleField);
        harness.check(true);
        field.setShort(this, (short)doubleField);
        harness.check(true);
        field.setInt(this, (int)doubleField);
        harness.check(true);
        field.setFloat(this, (float)doubleField);
        harness.check(true);
        field.setLong(this, (long)doubleField);
        harness.check(true);
        field.setDouble(this, doubleField);
        harness.check(true);
        field.set(this, new Byte((byte)doubleField));
        harness.check(true);
        field.set(this, new Character((char)doubleField));
        harness.check(true);
        field.set(this, new Short((short)doubleField));
        harness.check(true);
        field.set(this, new Integer((int)doubleField));
        harness.check(true);
        field.set(this, new Float((float)doubleField));
        harness.check(true);
        field.set(this, new Long((long)doubleField));
        harness.check(true);
        field.set(this, new Double(doubleField));
        harness.check(true);
    }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }
  }

  private void testGetIllegalArgument(TestHarness harness, Field field, boolean[] checks)
  {
    if (checks[0])
      try
        {
          field.getBoolean(this);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[1])
      try
        {
          field.getByte(this);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[2])
      try
        {
          field.getChar(this);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[3])
      try
        {
          field.getShort(this);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[4])
      try
        {
          field.getInt(this);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[5])
      try
        {
          field.getFloat(this);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[6])
      try
        {
          field.getLong(this);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[7])
      try
        {
          field.getDouble(this);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }
  }

  private void testSetIllegalArgument(TestHarness harness, Field field, boolean[] checks)
  {
    testSetObjectIllegalArgument(harness, field, checks);
    if (checks[0])
      try
        {
          field.setBoolean(this, booleanField);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[1])
      try
        {
          field.setByte(this, byteField);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[2])
      try
        {
          field.setChar(this, charField);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[3])
      try
        {
          field.setShort(this, shortField);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[4])
      try
        {
          field.setInt(this, intField);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[5])
      try
        {
          field.setFloat(this, floatField);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[6])
      try
        {
          field.setLong(this, longField);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[7])
      try
        {
          field.setDouble(this, doubleField);
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }
  }

  private void testSetObjectIllegalArgument(TestHarness harness, Field field, boolean[] checks)
  {
    if (checks[0])
      try
        {
          field.set(this, new Boolean(booleanField));
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[1])
      try
        {
          field.set(this, new Byte(byteField));
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[2])
      try
        {
          field.set(this, new Character(charField));
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[3])
      try
        {
          field.set(this, new Short(shortField));
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[4])
      try
        {
          field.set(this, new Integer(intField));
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[5])
      try
        {
          field.set(this, new Float(floatField));
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[6])
      try
        {
          field.set(this, new Long(longField));
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }

    if (checks[7])
      try
        {
          field.set(this, new Double(doubleField));
          harness.check(false);
        }
      catch (IllegalArgumentException _)
        {
          harness.check(true);
        }
      catch (Exception x)
        {
          harness.debug(x);
          harness.check(false);
        }
  }
}
