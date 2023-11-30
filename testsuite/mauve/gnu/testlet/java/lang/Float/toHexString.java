/* toHexString.java -- test Float.toHexString
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

package gnu.testlet.java.lang.Float;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * @author Tom Tromey
 * @author Pavel Tisnovsky (ptisnovs at redhat dot com)
 */
public class toHexString implements Testlet {
	public void test(TestHarness harness) {
        // special cases
		harness.check(Float.toHexString(Float.NaN), "NaN");
		harness.check(Float.toHexString(Float.POSITIVE_INFINITY), "Infinity");
		harness.check(Float.toHexString(Float.NEGATIVE_INFINITY), "-Infinity");
		harness.check(Float.toHexString(0.0f), "0x0.0p0");
		harness.check(Float.toHexString(-0.0f), "-0x0.0p0");

		// normalized values
		harness.check(Float.toHexString(0.125f),        "0x1.0p-3");
		harness.check(Float.toHexString(0.25f),         "0x1.0p-2");
		harness.check(Float.toHexString(0.5f),          "0x1.0p-1");
		harness.check(Float.toHexString(1.0f),          "0x1.0p0");
		harness.check(Float.toHexString(2.0f),          "0x1.0p1");
		harness.check(Float.toHexString(4.0f),          "0x1.0p2");
		harness.check(Float.toHexString(8.0f),          "0x1.0p3");
		harness.check(Float.toHexString(65536.0f),      "0x1.0p16");
        // 2^10
		harness.check(Float.toHexString(1024.0f),       "0x1.0p10");
        // 2^20
		harness.check(Float.toHexString(1048576.0f),    "0x1.0p20");
        // 2^30
		harness.check(Float.toHexString(1073741824.0f), "0x1.0p30");

		// subnormalized values
		harness.check(Float.toHexString( Float.intBitsToFloat(0x00800000)), "0x1.0p-126");
		harness.check(Float.toHexString(-Float.intBitsToFloat(0x00800000)), "-0x1.0p-126");

		// max and min representable values
		harness.check(Float.toHexString(Float.MAX_VALUE), "0x1.fffffep127");
		harness.check(Float.toHexString(Float.MIN_VALUE), "0x0.000002p-126");
	}
}
