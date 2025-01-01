/* Chain.java -- Test exception chaining
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

// Tags: JDK1.4

package gnu.testlet.java.lang.reflect.InvocationTargetException;

import java.lang.reflect.InvocationTargetException;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class Chain extends InvocationTargetException implements Testlet {

	public Chain() {
	}

	public Chain(Throwable targetException, String err) {
		super(targetException, err);
	}

	public Chain(Throwable targetException) {
		super(targetException);
	}

	public void test(TestHarness harness) {
		boolean ok = false;
		try {
			Throwable t = new Throwable();
			Chain c = new Chain();
			// Bogusly, the JDK does not allow this.
			c.initCause(c);
		} catch (IllegalStateException _) {
			harness.debug(_);
			ok = true;
		}
		harness.check(ok);
	}

}
