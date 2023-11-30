// Tags: JDK1.1

/* Copyright (C) 1999 Cygnus Solutions

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.io.DataInputStream;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class readLine implements Testlet
{
  private static void check(TestHarness harness, String input, String[] expected, int separator)
  {
    DataInputStream din = 
          new DataInputStream(new ByteArrayInputStream(input.getBytes()));
    for (int i = 0; i < expected.length; i++)
      {
        try
          {
            harness.check(din.readLine(), expected[i]);
            if (separator != -1)
                harness.check(din.read() == separator, "missing separator in: " + input);
          }
        catch(Exception x)
          {
            harness.fail("unexpected exception " + x);
          }
      }
    try
      {
        harness.check(din.readLine(), null);
      }
    catch(Exception x)
      {
        harness.fail("unexpected exception " + x);
      }
  }

  public void test (TestHarness harness)
  {
    check(harness, "", new String[] {}, -1);
    check(harness, "\n", new String[] { "" }, -1);
    check(harness, "\r", new String[] { "" }, -1);
    check(harness, "\r\n", new String[] { "" }, -1);
    check(harness, "\n\r", new String[] { "", "" }, -1);
    check(harness, "\r\nfoo", new String[] { "", "foo" }, -1);
    check(harness, "foo\r\nbar", new String[] { "foo", "bar" }, -1);
    check(harness, "foo\rbar", new String[] { "foo", "bar" }, -1);
    check(harness, "foo\nbar", new String[] { "foo", "bar" }, -1);
    check(harness, "foo\r\n:bar\n:", new String[] { "foo", "bar" }, ':');
    check(harness, "foo\r\n:bar\r:", new String[] { "foo", "bar" }, ':');
    check(harness, "foo\r\n:bar\r\n:", new String[] { "foo", "bar" }, ':');
  }
}
