// Tags: JDK1.1

// Copyright (C) 2000, 2005 Red Hat Inc.

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.util.Properties;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.Properties;
import java.io.*;

public class load implements Testlet
{
  public void test (TestHarness harness)
  {
    Properties p = new Properties ();
    p.setProperty ("a space", "a value");
    p.setProperty ("two  spaces", "spacy  ");

    ByteArrayOutputStream baos = new ByteArrayOutputStream ();
    try
      {
	p.store (baos, null);
      }
    catch (IOException _)
      {
      }

    Properties in = new Properties ();
    ByteArrayInputStream bais = new ByteArrayInputStream (baos.toByteArray ());

    try
      {
	in.load (bais);
      }
    catch (IOException _)
      {
      }

    // Sigh.  Work around gcj bug: Hashtable.equals doesn't work as of
    // Oct 5, 2000.
    // harness.check (in.equals (p));
    harness.check (in.size(), 2);
    harness.check (in.getProperty ("a space"), "a value");
    harness.check (in.getProperty ("two  spaces"), "spacy  ");

    // Tests copied from Kaffe's regression test
    p = new Properties();
    in = new Properties();

    for (int i = 0; i < EXPECT.length / 2; i++)
      p.setProperty(EXPECT[i * 2], EXPECT[i * 2 + 1]);

    try
      {
        in.load(new ByteArrayInputStream(INPUT.getBytes()));
      }
    catch (IOException _)
      {
      }
    harness.check (in.size(), p.size());
    for (int i = 0; i < EXPECT.length / 2; i++) {
      harness.check (in.getProperty (EXPECT[i * 2]),
                     p.getProperty (EXPECT[i * 2]));
    }

    // Classpath regression tests.
    harness.checkPoint("trailing backslash");
    p = new Properties();
    boolean ok = true;
    try
      {
	p.load(new ByteArrayInputStream("val = trailing backslash \\\n".getBytes()));
      }
    catch (IOException x)
      {
      }
    catch (Throwable x2)
      {
	ok = false;
      }
    harness.check(ok);
    harness.check(p.size(), 1);

    p = new Properties();
    ok = true;
    try
      {
	p.load(new ByteArrayInputStream("v\\".getBytes()));
      }
    catch (IOException x)
      {
      }
    catch (Throwable x2)
      {
	ok = false;
      }
    harness.check(ok);
    harness.check(p.size(), 1);
  }

  private static final String[] EXPECT = new String[] {
          "fooKey",                       "fooValue",
          "barKey",                       "override previous bar value",
          "noEqualsSign",                 "value",
          "spacesAroundEqualsSign",       "this is the value",
          "useColon",                     "colon's value",
          "key=contains=equals",          "value3",
          "key:contains:colons",          "value4",
          "key contains spaces",          "value5",
          "backslashes",                  "\r\n\t\\\"'",
          "lineContinuation",
                  "this is a line continuation times two",
          "unicodeString",
                  new String(new char[] { (char)0x1234, (char)0x5678 } ),
          "controlKey",
                  new String(new char[] { (char)1, } ),
          "keyWithEmptyStringAsValue",    ""
  };

  private static final String INPUT =
"fooKey=fooValue\n" +
"controlKey:\\u0001\n" +
"barKey=bar value\n" +
"barKey=override previous bar value\n" +
"noEqualsSign value\n" +
"spacesAroundEqualsSign   =  this is the value\n" +
"useColon : colon's value\n" +
"key\\=contains\\=equals=value3\n" +
"key\\:contains\\:colons=value4\n" +
"key\\ contains\\ spaces=value5\n" +
"backslashes = \\r\\n\\t\\\\\\\"\\'\n" +
"lineContinuation=this is a \\\n" +
"   line continuation \\\n" +
"   times two\n" +
"unicodeString=\\u1234\\u5678\n" +
"keyWithEmptyStringAsValue\n" +
"# a few blank lines follow\n" +
"\n" +
"\n" +
"# comment line\n" +
"      ## another comment line\n" +
"! another comment line\n" +
"      !! another comment line\n";

}
