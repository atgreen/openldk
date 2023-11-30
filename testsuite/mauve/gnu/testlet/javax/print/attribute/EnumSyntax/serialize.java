//Tags: JDK1.4
//Uses: WrongEnumSyntax CorrectEnumSyntax

//Copyright (C) 2005 Free Software Foundation, Inc.
//Written by Wolfgang Baer (WBaer@gmx.de)

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */


package gnu.testlet.javax.print.attribute.EnumSyntax;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;

/**
 * Tests serialization with EnumSyntax subclasses correctly
 * subclassed and one which is used to show correct exeption
 * handling of failures in readResolve user code.
 */
public class serialize implements Testlet
{

  public void test(TestHarness harness)
  {
    testSerializeErrors(harness);
    testSerializeNoErrors(harness);
  }
  
  /** Tests for exceptions in readResolve are thrown */
  private void testSerializeErrors(TestHarness harness)
  {
    WrongEnumSyntax test = WrongEnumSyntax.TEST;
    try
      {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        ObjectOutput out = new ObjectOutputStream(buffer);
        out.writeObject(test);
        out.close();

        ObjectInput in = new ObjectInputStream(new ByteArrayInputStream(
            buffer.toByteArray()));
        in.readObject();
        in.close();

        harness.check(false, "serialize error");
      }
    catch (Exception e)
      {        
        harness.check(true, "serialize error");
      }
  }
  
  /** Tests correct serialization */
  private void testSerializeNoErrors(TestHarness harness)
  {
    CorrectEnumSyntax inObj = CorrectEnumSyntax.TEST2;
    CorrectEnumSyntax outObj = null;
    try
      {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        ObjectOutput out = new ObjectOutputStream(buffer);
        out.writeObject(inObj);
        out.close();

        ObjectInput in = new ObjectInputStream(new ByteArrayInputStream(
            buffer.toByteArray()));
        outObj = (CorrectEnumSyntax) in.readObject();
        in.close();

        harness.check(true, "serialize no error");
        
        // test for identical object
        harness.check(inObj == outObj, "identity test");
        harness.check(inObj.equals(outObj), "equality test");
        harness.check(inObj.hashCode() == outObj.hashCode(), "hashcode test");
      }
    catch (Exception e)
      {        
        harness.check(false, "serialize no error");
      }
  }
}
