// Tags: not-a-test

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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;

/**
 * Helper class for getResource classes that checks resources when
 * URLClassLoader is set.
 */
public abstract class getResourceBase implements Testlet
{
  protected TestHarness harness;
  protected URLClassLoader ucl;

  /**
   * Checks that a resource exists in the <code>ucl</code> class loader.
   * The base string gives a hint about from which URL source it should
   * get loaded.
   * If noncanonical is true then it also checks non canonical ways
   * (using . and .. in the resource path) of accessing the resource.
   */
  protected void check(String resource, String base, boolean noncanonical)
  {
    URL u = ucl.getResource(resource);
    harness.debug(u != null ? u.toString() : null);
    String sep = base.endsWith(".jar") ? "!/" : "/";
    String fullpath = base + sep + resource;
    String r;
    if (u != null)
      {
	String f = u.getFile();
        File file = new File(u.getFile());

        // Canonize filenames. If the file is a directory, makes sure both filenames
        // end with a slash
        if (file.isDirectory()
            && fullpath.length() > 1
            && (fullpath.charAt(fullpath.length() - 1) != File.separator.charAt(0)))
          {
            fullpath = fullpath + File.separator;
          }
        
        if (file.isDirectory()
            && f.length() > 1
            && f.charAt(f.length() - 1) != File.separator.charAt(0))
          {
            f = f + File.separator;
          }
        
	int i = f.indexOf(fullpath);         
	if (i != -1)
	  r = f.substring(f.length() - fullpath.length());
	else
	  r = f;
      }
    else
      r = null;
    harness.check(r, fullpath, "URL file path ends with " + fullpath);

    u = ucl.getResource("no-" + resource);
    harness.debug(u != null ? u.toString() : null);
    harness.check(u == null, "no-" + resource);

    if (noncanonical)
      {
	int index = resource.lastIndexOf('/');
	String name, path, dir;
	if (index != -1)
	  {
	    name = resource.substring(index+1);
	    path = resource.substring(0, index);
	    index = path.lastIndexOf('/');
	    path = path + '/';
	    if (index != -1)
	      dir = path.substring(index);
	    else
	      dir = '/' + path;
	  }
	else
	  {
	    path = "";
	    name = resource;
	    dir = "";
	  }

	harness.debug(" == resource='" + resource
		      + "'; name='" + name
		      + "'; dir='" + dir
		      + "'; path='" + path + "'");

	u = ucl.getResource(path + '/' + "no-" + name);
	harness.debug(u != null ? u.toString() : null);
	harness.check(u == null, path + '/' + "no-" + name);

	u = ucl.getResource("./" + resource);
	harness.debug(u != null ? u.toString() : null);
	harness.check(u != null, "./" + resource);

	u = ucl.getResource(path + "./" + name);
	harness.debug(u != null ? u.toString() : null);
	harness.check(u != null, path + "./" + name);

	u = ucl.getResource(".\\" + resource);
	harness.debug(u != null ? u.toString() : null);
	harness.check(u == null, "no .\\" + resource);

	if (!dir.equals(""))
	  {
	    u = ucl.getResource(path + ".." + dir + name);
	    harness.debug(u != null ? u.toString() : null);
	    harness.check(u != null, path + ".." + dir + name);
	  }

	if (!path.equals(""))
	  {
	    u = ucl.getResource(path + "//" + name);
	    harness.debug(u != null ? u.toString() : null);
	    harness.check(u != null, path + "//" + name);
	  }

	u = ucl.getResource("\\" + resource);
	harness.debug(u != null ? u.toString() : null);
	harness.check(u == null, "no \\" + resource);

	u = ucl.getResource(":" + resource);
	harness.debug(u != null ? u.toString() : null);
	harness.check(u == null, "no :" + resource);
      }
  }
}
