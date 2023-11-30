// Simple tests of CompositeName.

// Copyright (C) 2001 Red Hat, Inc.

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
// Boston, MA 02111-1307, USA.

// Tags: JDK1.3

package gnu.testlet.javax.naming.CompositeName;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import javax.naming.*;

public class composite implements Testlet
{
  public void add (CompositeName n, String what)
  {
    // We always check the result of the add so we can just ignore the
    // exception.
    try
      {
	n.add (what);
      }
    catch (InvalidNameException _)
      {
      }
  }

  public void remove (CompositeName n, int pos)
  {
    // We always check the result of the remove so we can just ignore the
    // exception.
    try
      {
	n.remove (pos);
      }
    catch (InvalidNameException _)
      {
      }
  }

  public void test (TestHarness harness)
  {
    try
      {
	CompositeName cn1 = new CompositeName ();
	CompositeName cn2 = new CompositeName ("");

	// There are plenty of empty checks.
	harness.check (cn1.toString (), "", "empty CompositeName");
	harness.check (cn2.toString (), "");
	harness.check (cn1, cn2);
	harness.check (cn1.isEmpty ());
	harness.check (cn2.isEmpty ());
	harness.check (cn1.size (), 0);
	harness.check (cn2.size (), 0);
	harness.check (cn1.startsWith (cn2));
	harness.check (cn2.startsWith (cn1));

	add (cn1, "x");
	harness.check (! cn1.isEmpty (), "`x' CompositeName");
	harness.check (cn1.size (), 1);
	harness.check (cn1.get (0), "x");
	harness.check (cn1.toString (), "x");

	add (cn1, "y");
	harness.check (cn1.size (), 2, "`x/y' CompositeName");
	harness.check (cn1.get (0), "x");
	harness.check (cn1.get (1), "y");
	harness.check (cn1.toString (), "x/y");

	add (cn2, "");
	harness.check (cn2.size (), 1, "`/' CompositeName");
	harness.check (cn2.toString (), "/");

	add (cn2, "x");
	harness.check (cn2.size (), 2, "`/x' CompositeName");
	harness.check (cn2.toString (), "/x");

	add (cn2, "");
	harness.check (cn2.size (), 3, "`/x/' CompositeName");
	harness.check (cn2.toString (), "/x/");

	add (cn2, "y");
	harness.check (cn2.size (), 4, "`/x//y' CompositeName");
	harness.check (cn2.toString (), "/x//y");

	remove (cn2, 2);
	remove (cn2, 0);
	harness.check (cn2.size (), 2, "`x/y' CompositeName by removal");
	harness.check (cn2.toString (), "x/y");
	harness.check (cn1, cn2);

	add (cn1, "foo/bar");
	harness.check (cn1.size (), 3, "quoting rule");
	cn2 = new CompositeName (cn1.toString ());
	harness.check (cn2.size (), 3, "parsing with quoting");
	harness.check (cn2.get (2), "foo/bar");
	harness.check (cn1, cn2);

	cn2 = new CompositeName ("x/y/foo\\/bar");
	harness.check (cn1, cn2, "more parsing with quoting");
	cn2 = new CompositeName ("x/y/\"foo/bar\"");
	harness.check (cn1, cn2);

	cn1 = new CompositeName ("//");
	harness.check (cn1.size (), 2, "parsing `//'");
	harness.check (cn1.get (0), "");
	harness.check (cn1.get (1), "");
      }
    catch (NamingException _)
      {
	harness.debug (_);
	harness.fail ("NamingException caught");
      }
  }
}
