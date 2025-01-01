/* Tests15.java -- tests for 1.5 features of Long
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

package gnu.testlet.java.lang.Long;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class Tests15 implements Testlet {

	public void test(TestHarness harness) {
		harness.check(Long.SIZE, 64);
		harness.check(Long.valueOf(123456), new Long(123456));
		
		harness.checkPoint("bitCount");
		harness.check(Long.bitCount(0xffffffffffffffffL), 64);
		harness.check(Long.bitCount(0x5555555555555555L), 32);
		harness.check(Long.bitCount(0L), 0);
		harness.check(Long.bitCount(0x55555555aaaaaaaaL), 32);
		harness.check(Long.bitCount(0x1248842124188241L), 16);
		
		harness.checkPoint("rotateLeft");
		harness.check(Long.rotateLeft(0xffffffff00000000L, 16),
				0xffff00000000ffffL);
		harness.check(Long.rotateLeft(0x123456789abcdef0L, 128),
				0x123456789abcdef0L);
		
		harness.checkPoint("rotateRight");
		harness.check(Long.rotateRight(0xffffffff00000000L, 16),
				0x0000ffffffff0000L);
		harness.check(Long.rotateRight(0x123456789abcdef0L, 128),
				0x123456789abcdef0L);
		
		harness.checkPoint("highestOneBit");
		harness.check(Long.highestOneBit(0x00ff0000000000ffL),
				0x0080000000000000L);
		harness.check(Long.highestOneBit(0xf00000000000000fL),
				0x8000000000000000L);
		harness.check(Long.highestOneBit(0), 0);
		
		harness.checkPoint("numberOfLeadingZeros");
		harness.check(Long.numberOfLeadingZeros(0xf000000000000000L), 0);
		harness.check(Long.numberOfLeadingZeros(0x0505050505050505L), 5);
		harness.check(Long.numberOfLeadingZeros(1), 63);
		harness.check(Long.numberOfLeadingZeros(0), 64);
		
		harness.checkPoint("lowestOneBit");
		harness.check(Long.lowestOneBit(0x00ff0000000000ffL), 1);
		harness.check(Long.lowestOneBit(0xf00000000000000fL), 1);
		harness.check(Long.lowestOneBit(0xf000000000000f00L), 0x100);
		harness.check(Long.lowestOneBit(0), 0);
		
		harness.checkPoint("numberOfTrailingZeros");
		harness.check(Long.numberOfTrailingZeros(5), 0);
		harness.check(Long.numberOfTrailingZeros(0xf0), 4);
		harness.check(Long.numberOfTrailingZeros(0x8000000000000000L), 63);
		harness.check(Long.numberOfTrailingZeros(0), 64);
		
		harness.checkPoint("signum");
		harness.check(Long.signum(0), 0);
		harness.check(Long.signum(1), 1);
		harness.check(Long.signum(0x7fffffffffffffffL), 1);
		harness.check(Long.signum(0x8000000000000000L), -1);
		harness.check(Long.signum(-1L), -1);
		
		harness.checkPoint("reverseBytes");
		harness.check(Long.reverseBytes(0), 0);
		harness.check(Long.reverseBytes(0x123456789abcdef0L), 
				0xf0debc9a78563412L);

		harness.checkPoint("reverse");
		harness.check(Long.reverse(0), 0);
		harness.check(Long.reverse(-1), -1);
		harness.check(Long.reverse(0x5555555555555555L),
				0xaaaaaaaaaaaaaaaaL); 
	}
}
