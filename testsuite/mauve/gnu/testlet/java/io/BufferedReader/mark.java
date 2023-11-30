// Tags: JDK1.1

// Copyright (C) 2005 Free Software Foundation
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
// Boston, MA 02111-1307, USA.


package gnu.testlet.java.io.BufferedReader;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

/**
 * Small test based on a regression in GNU Classpath.
 */
public class mark implements Testlet
{
  public void test(TestHarness harness)
  {
    try
      {
	byte[] bs = new byte[] { 'a', 'b', 'c', 'd', 'e', 'f', 'g',
				 'h' ,'i', 'j', 'k', 'l', 'm', 'n' };
	ByteArrayInputStream bais = new ByteArrayInputStream(bs);
	InputStreamReader isr = new InputStreamReader(bais, "utf-8");
	BufferedReader br = new BufferedReader(isr);

	char[] cs = new char[4];
	br.mark(4);
	br.read(cs);
	harness.check('a', cs[0]);
	harness.check('b', cs[1]);
	harness.check('c', cs[2]);
	harness.check('d', cs[3]);
	br.reset();

	br.mark(12);
	harness.check('a', br.read());
	harness.check('b', br.read());
	harness.check('c', br.read());
	harness.check('d', br.read());
	harness.check('e', br.read());
	harness.check('f', br.read());
	harness.check('g', br.read());
	harness.check('h', br.read());
	harness.check('i', br.read());
	harness.check('j', br.read());
	harness.check('k', br.read());
	harness.check('l', br.read());
	harness.check('m', br.read());
	harness.check('n', br.read());

	harness.check(-1, br.read());
      }
    catch (IOException e)
      {
	harness.debug(e);
	harness.check(false);
      }
  }
}

