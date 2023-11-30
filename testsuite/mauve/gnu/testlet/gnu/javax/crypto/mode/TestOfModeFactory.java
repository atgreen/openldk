/* TestOfModeFactory.java -- 
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

package gnu.testlet.gnu.javax.crypto.mode;

import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.mode.ModeFactory;
import gnu.javax.crypto.mode.IMode;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import java.util.Iterator;

/**
 * Conformance tests for the ModeFactory implementation.
 */
public class TestOfModeFactory implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfModeFactory");
    String mode, cipher;
    int bs;
    IMode algorithm;
    for (Iterator mit = ModeFactory.getNames().iterator(); mit.hasNext();)
      {
        mode = (String) mit.next();
        for (Iterator cit = CipherFactory.getNames().iterator(); cit.hasNext();)
          {
            cipher = (String) cit.next();
            IBlockCipher ubc = CipherFactory.getInstance(cipher);
            for (Iterator cbs = ubc.blockSizes(); cbs.hasNext();)
              {
                bs = ((Integer) cbs.next()).intValue();
                try
                  {
                    algorithm = ModeFactory.getInstance(mode, ubc, bs);
                    harness.check(algorithm != null, "getInstance("
                                                     + String.valueOf(mode)
                                                     + ", "
                                                     + String.valueOf(cipher)
                                                     + ", "
                                                     + String.valueOf(8 * bs)
                                                     + ")");
                  }
                catch (InternalError x)
                  {
                    harness.debug(x);
                    harness.fail("TestOfModeFactory.getInstance("
                                 + String.valueOf(mode) + ", "
                                 + String.valueOf(cipher) + ", "
                                 + String.valueOf(8 * bs) + ")");
                  }
              }
          }
      }
  }
}