// Test of Float method floatToRawIntBits(Float).

// Copyright 2012 Red Hat, Inc.
// Written by Pavel Tisnovsky <ptisnovs@redhat.com>

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published 
// by the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software Foundation
// Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA

package gnu.testlet.java.lang.Float;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test the method Float.floatToRawIntBits(Float);
 */
public class floatToRawIntBits implements Testlet
{

  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
     harness.check(0x00000000, Float.floatToRawIntBits(0));
     harness.check(0x3f800000, Float.floatToRawIntBits(1));
     harness.check(0x40000000, Float.floatToRawIntBits(2));
     harness.check(0x447a0000, Float.floatToRawIntBits(1000));
     harness.check(0xbf800000, Float.floatToRawIntBits(-1));
     harness.check(0xc0000000, Float.floatToRawIntBits(-2));
     harness.check(0xc47a0000, Float.floatToRawIntBits(-1000));
     harness.check(0x7f800000, Float.floatToRawIntBits(Float.POSITIVE_INFINITY));
     harness.check(0xff800000, Float.floatToRawIntBits(Float.NEGATIVE_INFINITY));
     harness.check(0x7f7fffff, Float.floatToRawIntBits(Float.MAX_VALUE));
     harness.check(0x00000001, Float.floatToRawIntBits(Float.MIN_VALUE));
     harness.check(0xff7fffff, Float.floatToRawIntBits(-Float.MAX_VALUE));
     harness.check(0x80000001, Float.floatToRawIntBits(-Float.MIN_VALUE));
     harness.check(0x7fc00000, Float.floatToRawIntBits(Float.NaN));
  }
}

