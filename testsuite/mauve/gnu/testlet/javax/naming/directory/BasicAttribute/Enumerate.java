/* Enumerate.java -- test BasicAttribute enumeration
   Copyright (C) 2006 Red Hat, Inc.
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.3

package gnu.testlet.javax.naming.directory.BasicAttribute;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.BasicAttribute;

public class Enumerate implements Testlet
{
	public void test(TestHarness harness)
	{
		BasicAttribute b = new BasicAttribute("test");
		b.add("two");
		b.add("three");

		boolean ok = true;
		NamingEnumeration e = null;
		try {
			e = b.getAll();
		} catch (NamingException _) {
			harness.debug(_);
			ok = false;
		}
		harness.check(ok);

		harness.check(e.nextElement(), "two");
		harness.check(e.nextElement(), "three");
		harness.check(! e.hasMoreElements());
	}
}
