/* TestOfISO10126.java
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

package gnu.testlet.gnu.javax.crypto.pad;

import gnu.javax.crypto.pad.IPad;
import gnu.javax.crypto.pad.PadFactory;
import gnu.javax.crypto.pad.WrongPaddingException;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Basic tests for the ISO-10126 padding scheme.
 */
public class TestOfISO10126
    implements Testlet
{
  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfISO10126");
    try
      {
        IPad algorithm = PadFactory.getInstance("iso10126");
        byte[] in = new byte[1024];
        for (int bs = 2; bs < 256; bs++)
          if (! padderTest1BlockSize(algorithm, bs, in))
            harness.fail("ISO10126 padder failed for block-size = " + bs);
        harness.check(true, "ISO10126 padder OK");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfISO10126");
      }
  }

  private boolean padderTest1BlockSize(IPad algorithm, int size, byte[] buffer)
  {
    final int offset = 5;
    final int limit = buffer.length;
    byte[] padding;
    int len;

    algorithm.init(size);
    for (int i = 0; i < limit - offset - size; i++)
      {
        padding = algorithm.pad(buffer, offset, i);
        len = padding.length;
        if (((i + len) % size) != 0)
          return false;
        System.arraycopy(padding, 0, buffer, offset + i, len);
        try
          {
            if (len != algorithm.unpad(buffer, offset, i + len))
              return false;
          }
        catch (WrongPaddingException x)
          {
            return false;
          }
      }
    algorithm.reset();
    return true;
  }
}
