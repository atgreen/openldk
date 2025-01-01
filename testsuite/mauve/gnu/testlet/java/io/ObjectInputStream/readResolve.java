//Tags: JDK1.1
//Uses: ReadResolveHelper

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


package gnu.testlet.java.io.ObjectInputStream;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;

/**
 * Tests readResolve implementation. Tests are done to see if readResolve is
 * actually invoked and if all exception types are passed through to the caller.
 */
public class readResolve implements Testlet
{
  public void test(TestHarness harness)
  {
    ReadResolveHelper test, test_deserialized;
    ByteArrayOutputStream buffer;
    ObjectOutput out;
    ObjectInput in;

    harness.checkPoint("readResolve called");
    test = new ReadResolveHelper(5); // tests default
    test_deserialized = null;

    try
      {
        buffer = new ByteArrayOutputStream();
        out = new ObjectOutputStream(buffer);
        out.writeObject(test);
        out.close();
        in = new ObjectInputStream(new ByteArrayInputStream(
            buffer.toByteArray()));
        test_deserialized = (ReadResolveHelper) in.readObject();
        in.close();

        harness.check(test_deserialized.value, 4);
      }
    catch (Throwable e)
      {
        harness.check(false);
      }

    harness.checkPoint("error thrown");
    test = new ReadResolveHelper(1); // tests case 1
    test_deserialized = null;

    try
      {
        buffer = new ByteArrayOutputStream();
        out = new ObjectOutputStream(buffer);
        out.writeObject(test);
        out.close();
        in = new ObjectInputStream(new ByteArrayInputStream(
            buffer.toByteArray()));
        test_deserialized = (ReadResolveHelper) in.readObject();
        in.close();

        harness.check(false);
      }
    catch (Throwable e)
      {
        harness.check(true);
      }

    harness.checkPoint("runtime exception thrown");
    test = new ReadResolveHelper(2); // tests case 2
    test_deserialized = null;

    try
      {
        buffer = new ByteArrayOutputStream();
        out = new ObjectOutputStream(buffer);
        out.writeObject(test);
        out.close();
        in = new ObjectInputStream(new ByteArrayInputStream(
            buffer.toByteArray()));
        test_deserialized = (ReadResolveHelper) in.readObject();
        in.close();

        harness.check(false);
      }
    catch (Throwable e)
      {
        harness.check(true);
      }

    harness.checkPoint("InvalidObjectException thrown");
    test = new ReadResolveHelper(3); // tests case 3
    test_deserialized = null;

    try
      {
        buffer = new ByteArrayOutputStream();
        out = new ObjectOutputStream(buffer);
        out.writeObject(test);
        out.close();
        in = new ObjectInputStream(new ByteArrayInputStream(
            buffer.toByteArray()));
        test_deserialized = (ReadResolveHelper) in.readObject();
        in.close();

        harness.check(false);
      }
    catch (Throwable e)
      {
        harness.check(true);
      }
  }
}
