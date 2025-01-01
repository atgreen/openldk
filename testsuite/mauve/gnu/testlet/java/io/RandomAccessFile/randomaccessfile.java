// Tags: JDK1.1

// Copyright (C) 2005 Free Software Foundation, Inc.
// Written by Dalibor Topic (robilad@kaffe.org)

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

package gnu.testlet.java.io.RandomAccessFile;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.RandomAccessFile;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class randomaccessfile implements Testlet
{
    /**
     * This test checks for a FileNotFoundException being thrown
     * when the input parameter to the constructor is a directory
     */
  public void test (TestHarness harness)
  {
    String tmpfile = ".";

    try
      {
	new RandomAccessFile(tmpfile, "r");
	harness.check(false, "Failed to throw FileNotFoundException");
      }
    catch(FileNotFoundException e)
      {
	harness.check(true, "thrown FileNotFoundException for directory parameter");
      }
    catch(Throwable t)
      {
	harness.fail("Unknown Throwable caught");
	harness.debug(t);
      }

    final File f = new File(tmpfile);

    try
      {
	new RandomAccessFile(f, "r");
	harness.check(false, "Failed to throw FileNotFoundException");
      }
    catch(FileNotFoundException e)
      {
	harness.check(true, "thrown FileNotFoundException for directory parameter");
      }
    catch(Throwable t)
      {
	harness.fail("Unknown Throwable caught");
	harness.debug(t);
      }
  }
}
