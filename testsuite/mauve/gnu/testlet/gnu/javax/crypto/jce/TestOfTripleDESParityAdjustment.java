/* TestOfTripleDESParityAdjustment.java
   Copyright (C) 2006 Free Software Foundation, Inc.
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

// Tags: GNU-CRYPTO JDK1.4

package gnu.testlet.gnu.javax.crypto.jce;

import gnu.javax.crypto.cipher.TripleDES;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Conformance tests for DES and TripleDES key parity adjustment methods.
 */
public class TestOfTripleDESParityAdjustment
    implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("test()");
    byte[] kBytes = new byte[24];
    for (int i = 0; i < kBytes.length; i++)
      kBytes[i] = (byte)(i + 1);

    TripleDES.adjustParity(kBytes, 0);
    harness.check(TripleDES.isParityAdjusted(kBytes, 0), "Parity MUST be adjusted");
  }
}
