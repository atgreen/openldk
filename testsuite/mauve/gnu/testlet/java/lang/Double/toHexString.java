/* toHexString.java -- test Double.toHexString
   Copyright (C) 2005 Red Hat, Inc.
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

package gnu.testlet.java.lang.Double;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * @author Tom Tromey
 * @author Pavel Tisnovsky (ptisnovs at redhat dot com)
 */
public class toHexString implements Testlet {

	public void test(TestHarness harness) {
        // special cases
		harness.check(Double.toHexString(Double.NaN), "NaN");
		harness.check(Double.toHexString(Double.NEGATIVE_INFINITY), "-Infinity");
		harness.check(Double.toHexString(Double.POSITIVE_INFINITY), "Infinity");
		harness.check(Double.toHexString(0.0), "0x0.0p0");
		harness.check(Double.toHexString(-0.0), "-0x0.0p0");

		// normalized values
		harness.check(Double.toHexString(0.125),        "0x1.0p-3");
		harness.check(Double.toHexString(0.25),         "0x1.0p-2");
		harness.check(Double.toHexString(0.5),          "0x1.0p-1");
		harness.check(Double.toHexString(1.0),          "0x1.0p0");
		harness.check(Double.toHexString(2.0),          "0x1.0p1");
		harness.check(Double.toHexString(4.0),          "0x1.0p2");
		harness.check(Double.toHexString(8.0),          "0x1.0p3");
		harness.check(Double.toHexString(65536.0),      "0x1.0p16");
        // 2^10
		harness.check(Double.toHexString(1024.0),       "0x1.0p10");
        // 2^20
		harness.check(Double.toHexString(1048576.0),    "0x1.0p20");
        // 2^30
		harness.check(Double.toHexString(1073741824.0), "0x1.0p30");

		// well-known problematic case
		//harness.check(Double.toHexString(0.1), "0x1.999999999999ap-4");

		// subnormalized values
		harness.check(Double.toHexString( Double.longBitsToDouble(0x0010000000000000L)), "0x1.0p-1022");
		harness.check(Double.toHexString(-Double.longBitsToDouble(0x0010000000000000L)), "-0x1.0p-1022");

		// max and min representable values
		harness.check(Double.toHexString( Double.MAX_VALUE), "0x1.fffffffffffffp1023");
		harness.check(Double.toHexString(-Double.MAX_VALUE), "-0x1.fffffffffffffp1023");
		harness.check(Double.toHexString( Double.MIN_VALUE), "0x0.0000000000001p-1022");
		harness.check(Double.toHexString(-Double.MAX_VALUE), "-0x1.fffffffffffffp1023");
	}
}
