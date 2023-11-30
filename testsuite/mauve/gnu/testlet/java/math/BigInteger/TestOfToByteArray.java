/* TestOfToByteArray.java
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

// Tags: JDK1.2

package gnu.testlet.java.math.BigInteger;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.math.BigInteger;
import java.util.Arrays;

/**
 * Test case for report in http://gcc.gnu.org/ml/java/2006-08/msg00090.html.
 */
public class TestOfToByteArray
    implements Testlet
{
  private static final byte[] BYTES = { 32, 33, 34, 35, 36, 37, 0, 0, 0, 0, 0, 0 };

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfToByteArray");
    try
    {
      BigInteger x = new BigInteger(BYTES);
      harness.verbose("*** x = 0x" + x.toString(16));
      byte[] ba = x.toByteArray();
      harness.check(Arrays.equals(ba, BYTES), true, "Byte arrays MUST be equal");
    }
  catch (Exception x)
    {
      harness.debug(x);
      harness.fail("TestOfToByteArray: " + x);
    }
  }
}
