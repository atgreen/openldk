/* TestOfCipherFactory.java -- 
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

package gnu.testlet.gnu.javax.crypto.cipher;

import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import java.util.Iterator;

/**
 * Conformance tests for the CipherFactory implementation.
 */
public class TestOfCipherFactory implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfCipherFactory");
    String cipher;
    IBlockCipher algorithm;
    for (Iterator it = CipherFactory.getNames().iterator(); it.hasNext();)
      {
        cipher = (String) it.next();
        try
          {
            algorithm = null;
            algorithm = CipherFactory.getInstance(cipher);
            harness.check(algorithm != null, "getInstance("
                                             + String.valueOf(cipher) + ")");
          }
        catch (InternalError x)
          {
            harness.debug(x);
            harness.fail("TestOfCipherFactory.getInstance("
                         + String.valueOf(cipher) + ")");
          }
      }
  }
}