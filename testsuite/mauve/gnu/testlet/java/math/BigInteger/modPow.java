/* modPow.java -- tests for modPow
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

// Tags: JDK1.1

package gnu.testlet.java.math.BigInteger;

import java.math.BigInteger;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class modPow implements Testlet {

	public void test(TestHarness harness) {
		BigInteger ten = BigInteger.valueOf(10);
		BigInteger three = BigInteger.valueOf(3);
		BigInteger five = BigInteger.valueOf(5);
		BigInteger minusFive = five.negate();
		BigInteger seven = BigInteger.valueOf(7);

		harness.check(three.modPow(five, ten), three);
		harness.check(three.modPow(minusFive, ten), seven);
		harness.check(three.modPow(three.negate(), ten), three);
	}

}
