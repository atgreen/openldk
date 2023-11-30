// Tags: JDK1.1

// Copyright (C) 1999, 2000 Free Software Foundation

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

package gnu.testlet.java.util.zip.ZipInputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.zip.*;
import java.io.*;

public class close implements Testlet
{
  private String readall (InputStream in)
  {
    StringBuffer sb = new StringBuffer ();
    byte[] buf = new byte[512];
    int n;
    try
      {
	in.close ();
	while ((n = in.read(buf)) > 0)
	  sb.append(new String (buf, 0, n, "8859_1"));
      }
    catch (IOException _)
      {
      }
    return sb.toString ();
  }

  public String read_a_file (ZipInputStream zis)
  {
    try
      {
	ZipEntry ze = zis.getNextEntry();
	if (ze == null)
	  return "done";
	return readall (zis);
      }
    catch (IOException _)
      {
	return "";
      }
  }

  public void read_contents (TestHarness harness, ZipInputStream zis)
  {
    String s = read_a_file (zis);
    harness.check (s, "");
    s = read_a_file (zis);
    harness.check (s, "");
    s = read_a_file (zis);
    harness.check (s, "");
  }

  public void test (TestHarness harness)
  {
    harness.checkPoint ("reading zip file");
    try
      {
	read_contents (harness,
		       new ZipInputStream (harness.getResourceStream ("gnu#testlet#java#util#zip#ZipInputStream#reference.zip")));
      }
    catch (gnu.testlet.ResourceNotFoundException _)
      {
	// FIXME: all tests should fail.
      }

    harness.checkPoint ("writing and re-reading");
  }
}
