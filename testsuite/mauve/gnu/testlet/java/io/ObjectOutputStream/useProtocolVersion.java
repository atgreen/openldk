//Tags: JDK1.2

//Copyright (C) 2006 Free Software Foundation, Inc.
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
//the Free Software Foundation, 51 Franklin Street, Fifth Floor,
//Boston, MA, 02110-1301 USA.

package gnu.testlet.java.io.ObjectOutputStream;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamConstants;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the correct behaviour of useProtocolVersion
 */
public class useProtocolVersion implements Testlet
{
  public void test(TestHarness harness)
  {
    try
      {
        String toSerialize = "Hello";

        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        ObjectOutputStream out = new ObjectOutputStream(bytes);

        try
          {
            // setting must be allowed
            out.useProtocolVersion(ObjectStreamConstants.PROTOCOL_VERSION_1);
            harness.check(true);
          }
        catch (RuntimeException e)
          {
            harness.check(false);
          }

        // if only normal data is written
        // a subsequent call to useProtocolVersion must succeed also
        out.writeInt(4);

        try
          {
            out.useProtocolVersion(ObjectStreamConstants.PROTOCOL_VERSION_1);
            harness.check(true);
          }
        catch (IllegalStateException e)
          {
            harness.check(false);
          }

        // as soon as the first object is serialized
        // subsequent calls must throw an exception
        out.writeObject(toSerialize);

        try
          {
            out.useProtocolVersion(ObjectStreamConstants.PROTOCOL_VERSION_1);
            harness.check(false);
          }
        catch (IllegalStateException e)
          {
            harness.check(true);
          }

        // use new stream
        out = new ObjectOutputStream(bytes);

        // wrong versions must throw IllegalArgumentException
        try
          {
            out.useProtocolVersion(4);
            harness.check(false);
          }
        catch (IllegalArgumentException e)
          {
            harness.check(true);
          }
      }
    catch (IOException e)
      {
        harness.fail("Unexpected exception occured.");
        harness.debug(e);
      }
  }
}
