/* ulp.java -- Test the ulp method
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

// Tags: JDK1.5

package gnu.testlet.java.lang.Math;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class ulp implements Testlet {

	public void test(TestHarness harness) {
		harness.check(Math.ulp(0.0), Double.MIN_VALUE);
		harness.check(Math.ulp(Double.NaN), Double.NaN);
		harness.check(Math.ulp(Double.MAX_VALUE), Math.pow(2.0, 971));
		harness.check(Math.ulp(Double.NEGATIVE_INFINITY), Double.POSITIVE_INFINITY);
		harness.check(Math.ulp(Double.MIN_VALUE), Double.MIN_VALUE);

		harness.check(Math.ulp(0.0f), Float.MIN_VALUE);
		harness.check(Math.ulp(Float.NaN), Float.NaN);
		harness.check(Math.ulp(Float.MAX_VALUE), Math.pow(2.0, 104));
		harness.check(Math.ulp(Float.NEGATIVE_INFINITY), Float.POSITIVE_INFINITY);
		harness.check(Math.ulp(Float.MIN_VALUE), Float.MIN_VALUE);
	}
}
