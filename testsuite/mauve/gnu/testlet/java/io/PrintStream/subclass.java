// Test simple forms of MessageFormat formatting.

// Copyright (c) 2001, 2002  Red Hat, Inc.
// Written by Tom Tromey <tromey@cygnus.com>

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

// Tags: JDK1.1

package gnu.testlet.java.io.PrintStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;

public class subclass extends PrintStream implements Testlet
{
  public subclass ()
  {
    // Use dummy OutputStream
    super (new ByteArrayOutputStream ());
  }

  public void setOutput (OutputStream x)
  {
    this.out = x;
  }

  public void test (TestHarness harness)
  {
    boolean ok = true;
    try
      {
	// Set the underlying output stream and then write to it.  We
	// should get the right results.
	ByteArrayOutputStream b = new ByteArrayOutputStream ();
	setOutput (b);
	print ("foo");
	flush ();
	ok = b.toString().equals ("foo");
      }
    catch (Throwable _)
      {
	ok = false;
      }
    harness.check (ok);
  }
}
