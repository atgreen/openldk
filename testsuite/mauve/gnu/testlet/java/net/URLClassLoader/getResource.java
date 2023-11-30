// Tags: JDK1.2
// Uses: getResourceBase

// Copyright (C) 2002, 2006 Free Software Foundation, Inc.
// Written by Mark Wielaard (mark@klomp.org)

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

package gnu.testlet.java.net.URLClassLoader;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import java.net.URL;
import java.net.URLClassLoader;

import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;

import gnu.testlet.TestHarness;

public class getResource extends getResourceBase
{
  private File tmpdir, tmpfile, subtmpdir, subtmpfile;
  private String jarPath;

  private void setup() throws IOException
  {
    // Setup
    String tmp = harness.getTempDirectory();
    tmpdir = new File(tmp + File.separator + "mauve-testdir");
    if (!tmpdir.mkdir() && !tmpdir.exists())
      throw new IOException("Could not create: " + tmpdir);

    tmpfile = new File(tmpdir, "testfile");
    if (!tmpfile.delete() && tmpfile.exists())
      throw new IOException("Could not remove (old): " + tmpfile);
    tmpfile.createNewFile();

    subtmpdir = new File(tmpdir, "testdir");
    if (!subtmpdir.mkdir() && !subtmpdir.exists())
      throw new IOException("Could not create: " + subtmpdir);

    subtmpfile = new File(subtmpdir, "test");
    if (!subtmpfile.delete() && subtmpfile.exists())
      throw new IOException("Could not remove (old): " + subtmpfile);
    subtmpfile.createNewFile();

    jarPath = tmp + File.separator + "m.jar";
    FileOutputStream fos = new FileOutputStream(jarPath);
    JarOutputStream jos = new JarOutputStream(fos);

    JarEntry je = new JarEntry("jresource");
    jos.putNextEntry(je);
    jos.write(new byte[256]);

    je = new JarEntry("path/in/jar/");
    jos.putNextEntry(je);
    je = new JarEntry("path/in/jar/file");
    jos.putNextEntry(je);
    jos.write(new byte[256]);
    jos.close();
    fos.close();
  }

  private void tearDown()
  {
    if (tmpdir != null && tmpdir.exists())
      {
	if (tmpfile != null && tmpfile.exists())
	  tmpfile.delete();

	if (subtmpdir != null && subtmpdir.exists())
	  {
	    if (subtmpfile != null && subtmpfile.exists())
	      subtmpfile.delete();
	    subtmpdir.delete();
	  }
	tmpdir.delete();
      }

    if (jarPath != null)
      new File(jarPath).delete();
  }

  public void test (TestHarness h)
  {
    harness = h;

    try
      {
	setup();

	URL[] urls = new URL[2];
	urls[0] = tmpdir.toURL();
	urls[1] = new URL("file://" + jarPath);
	ucl = URLClassLoader.newInstance(urls);

        // Check that the root resource can be found
        // It is valid as a directory
	URL u = ucl.getResource(".");
	harness.debug(u != null ? u.toString() : null);
	harness.check(u != null, "Checking .");

        // Check that the parent directory can not be found
        // It is invalid as one shouldn't be able to escape the "url root"
	u = ucl.getResource("..");
	harness.debug(u != null ? u.toString() : null);
	harness.check(u == null, "Checking ..");

        // Check that the current directory can not be found
        // It is invalid because at one point, we're escaping the "url root"
        u = ucl.getResource("../mauve-testdir");
        harness.debug(u != null ? u.toString() : null);
        harness.check(u == null, "Checking ../mauve-testdir");
        
        // Check that the current directory can not be found
        // It is invalid because at one point, we're walking into an invalid directory
        u = ucl.getResource("mauve-testdir/..");
        harness.debug(u != null ? u.toString() : null);
        harness.check(u == null, "Checking mauve-testdir/..");

        // Check that the current directory can not be found
        // It is invalid because at one point, we're walking into an invalid directory
        u = ucl.getResource("mauve-testdir/../");
        harness.debug(u != null ? u.toString() : null);
        harness.check(u == null, "Checking mauve-testdir/../");

        // Check that the current directory can be found
        // It is valid because we're walking into an valid directory and back
        u = ucl.getResource("testdir/..");
        harness.debug(u != null ? u.toString() : null);
        harness.check(u != null, "Checking testdir/..");

        // Check that the current directory can be found
        // It is valid because we're walking into an valid directory and back
        u = ucl.getResource("testdir/../");
        harness.debug(u != null ? u.toString() : null);
        harness.check(u != null, "Checking testdir/../");        
        
        // Check that the testdir subdirectory can not be found
        // It is invalid because at one point, we're walking into an invalid directory
        u = ucl.getResource("mauve-testdir/../testdir");
        harness.debug(u != null ? u.toString() : null);
        harness.check(u == null, "Checking mauve-testdir/../testdir");        
        
        // Check that the testdir subdirectory can be found
        // It is valid because we're walking into an valid directory and back
        // and into a valid directory again
        u = ucl.getResource("testdir/../testdir");
        harness.debug(u != null ? u.toString() : null);
        harness.check(u != null, "Checking testdir/../testdir");        
        
	check("testfile", "mauve-testdir", true);
	check("testdir", "mauve-testdir", true);
	check("testdir/test", "mauve-testdir", true);
	check("jresource", "m.jar", false);
	check("path/in/jar/file", "m.jar", false);
	check("path/in/jar", "m.jar", false);
      }
    catch(IOException ioe)
      {
	harness.fail("Unexpected exception: " + ioe);
	harness.debug(ioe);
      }
    finally
      {
	tearDown();
      }
  }
}
