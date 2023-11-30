/* Tests15.java -- tests for 1.5 features of Integer
   Copyright (C) 2005, 2008 Red Hat, Inc.
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

package gnu.testlet.java.lang.Integer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class Tests15 implements Testlet {

	public void test(TestHarness harness) {
		harness.check(Integer.SIZE, 32);
		harness.check(Integer.valueOf(123456), new Integer(123456));
		
		harness.checkPoint("bitCount");
		harness.check(Integer.bitCount(0xffffffff), 32);
		harness.check(Integer.bitCount(0x55555555), 16);
		harness.check(Integer.bitCount(0), 0);
		harness.check(Integer.bitCount(0x5555aaaa), 16);
		harness.check(Integer.bitCount(0x12488421), 8);
		
		harness.checkPoint("rotateLeft");
		harness.check(Integer.rotateLeft(0xffff0000, 8), 0xff0000ff);
		harness.check(Integer.rotateLeft(0x12345678, 64), 0x12345678);
		
		harness.checkPoint("rotateRight");
		harness.check(Integer.rotateRight(0xffff0000, 8), 0x00ffff00);
		harness.check(Integer.rotateRight(0x12345678, 64), 0x12345678);
		
		harness.checkPoint("highestOneBit");
		harness.check(Integer.highestOneBit(0x0ff000ff), 0x08000000);
		harness.check(Integer.highestOneBit(0xf000000f), 0x80000000);
		harness.check(Integer.highestOneBit(0), 0);
		
		harness.checkPoint("numberOfLeadingZeros");
		harness.check(Integer.numberOfLeadingZeros(0xf0000000), 0);
		harness.check(Integer.numberOfLeadingZeros(0x05050505), 5);
		harness.check(Integer.numberOfLeadingZeros(1), 31);
		harness.check(Integer.numberOfLeadingZeros(0), 32);
		
		harness.checkPoint("lowestOneBit");
		harness.check(Integer.lowestOneBit(0x0ff000ff), 1);
		harness.check(Integer.lowestOneBit(0xf000000f), 1);
		harness.check(Integer.lowestOneBit(0xf0000f00), 0x100);
		harness.check(Integer.lowestOneBit(0), 0);
		
		harness.checkPoint("numberOfTrailingZeros");
		harness.check(Integer.numberOfTrailingZeros(5), 0);
		harness.check(Integer.numberOfTrailingZeros(0xf0), 4);
		harness.check(Integer.numberOfTrailingZeros(0x80000000), 31);
		harness.check(Integer.numberOfTrailingZeros(0), 32);
		
		harness.checkPoint("signum");
		harness.check(Integer.signum(0), 0);
		harness.check(Integer.signum(1), 1);
		harness.check(Integer.signum(0x7fffffff), 1);
		harness.check(Integer.signum(0x80000000), -1);
		harness.check(Integer.signum(-1), -1);
		
		harness.checkPoint("reverseBytes");
		harness.check(Integer.reverseBytes(0), 0);
		harness.check(Integer.reverseBytes(0x12345678), 0x78563412);

		harness.checkPoint("reverse");
		harness.check(Integer.reverse(0), 0);
		harness.check(Integer.reverse(-1), -1);
		harness.check(Integer.reverse(0x55555555), 0xaaaaaaaa);
	}
}
