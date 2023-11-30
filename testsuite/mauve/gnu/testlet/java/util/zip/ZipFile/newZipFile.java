// Tags: JDK1.1

// Copyright (C) 2003 Free Software Foundation
// Contributed by Mark Wielaard (mark@klomp.org)

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

package gnu.testlet.java.util.zip.ZipFile;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;
import java.util.zip.*;

public class newZipFile implements Testlet
{
  private TestHarness harness;
  private String zipname, nozipname;

  private File tmpdir, tmpfile, tmpfile2;

  private void setup() throws IOException
  {
    String tmp = harness.getTempDirectory();
    tmpdir = new File(tmp + File.separator + "mauve-testdir");
    if (!tmpdir.mkdir() && !tmpdir.exists())
      throw new IOException("Could not create: " + tmpdir);
    
    tmpfile = new File(tmpdir, "test.zip");
    if (!tmpfile.delete() && tmpfile.exists())
      throw new IOException("Could not remove (old): " + tmpfile);
    tmpfile.createNewFile();

    ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(tmpfile));
    zos.putNextEntry(new ZipEntry("dummy"));
    zos.close();

    zipname = tmpfile.toString();

    tmpfile2 = new File(tmpdir, "test.tmp");
    if (!tmpfile2.delete() && tmpfile2.exists())
      throw new IOException("Could not remove (old): " + tmpfile2);
    tmpfile2.createNewFile();

    FileOutputStream fos = new FileOutputStream(tmpfile2);
    fos.write(new byte [] { (byte) 0, (byte) 1, (byte) 2, (byte) 3, (byte) 4 });
    fos.close();

    nozipname = tmpfile2.toString();
  }

  private void tearDown()
  {
    if (tmpdir != null && tmpdir.exists())
      {
        if (tmpfile != null && tmpfile.exists())
          tmpfile.delete();
        if (tmpfile2 != null && tmpfile2.exists())
          tmpfile2.delete();
	tmpdir.delete();
      }
  }

  public void test (TestHarness harness)
  {
    this.harness = harness;
    try
      {
	setup();
	ZipFile zf = new ZipFile(zipname);
	harness.check(zf.getName(), zipname);

	zf = new ZipFile(new File(zipname));
	harness.check(zf.getName(), zipname);

	boolean exception;
	try
	  {
	    new ZipFile((String)null);
	    exception = false;
	  }
	catch (NullPointerException _)
	  {
	    exception = true;
	  }
	harness.check(exception, "name is null");

	try
	  {
	    new ZipFile((File)null);
	    exception = false;
	  }
	catch (NullPointerException _)
	  {
	    exception = true;
	  }
	harness.check(exception, "name is null");
	
	try
	  {
	    new ZipFile(nozipname);
	    exception = false;
	  }
	catch (ZipException _)
	  {
	    exception = true;
	  }
	harness.check(exception, "non-zipfile gets rejected");
      }
    catch (IOException ioe)
      {
	harness.check(false, ioe.toString());
	harness.debug(ioe);
      }
    finally
      {
	tearDown();
      }
  }
}
