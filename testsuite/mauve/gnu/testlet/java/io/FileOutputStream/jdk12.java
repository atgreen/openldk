// Tags: JDK1.2

// Copyright (C) 2002 Free Software Foundation, Inc.
// Written by Daryl O. Lee (dolee@sources.redhat.com)
// Tests only changes in exceptions thrown by constructors.

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


package gnu.testlet.java.io.FileOutputStream;

import java.io.FileOutputStream;
import java.io.FileNotFoundException;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class jdk12 implements Testlet
{
  public void test (TestHarness harness)
  {
	// pick a directory we know doesn't exist
    String tmpfile = "./asdfghj/mauve-fileout.tst";
    try
      {
		new FileOutputStream(tmpfile);
		harness.fail("No exception thrown");
      }
    catch(FileNotFoundException t)
      {
		harness.check(true, "new(string) Threw correct exception");
      }
    try
      {
		new FileOutputStream(tmpfile, true);
		harness.fail("No exception thrown");
      }
    catch(FileNotFoundException t)
      {
		harness.check(true, "new(string, boolean) Threw correct exception");
      }


  }
}
