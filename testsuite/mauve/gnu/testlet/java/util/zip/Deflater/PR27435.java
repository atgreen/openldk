// Tags: JDK1.2

// Copyright (C) 2006 Andrew John Hughes <gnu_andrew@member.fsf.org>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version. 

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.util.zip.Deflater;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.zip.Deflater;

/*
 * This test is based on PR27435, which reported
 * an ArrayOutOfBoundsException when using the DeflateFast
 * method.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
public class PR27435
  implements Testlet
{
  /**
   * Buffer must be big enough to go over the internal
   * buffer size of 16384.  Currently somewhere above
   * 1024*1024*4.
   */
  public static final int BUFFER_SIZE = 1024*1024*5;

  public void test(TestHarness harness)
  {
    byte[] inputBytes = new byte[BUFFER_SIZE];
    byte[] compressedData = new byte[BUFFER_SIZE];

    /* Fill array with bytes */
    for (int a = 0; a < inputBytes.length; ++a)
      inputBytes[a] = 1;

    Deflater deflater = new Deflater(Deflater.BEST_SPEED);
    deflater.setInput(inputBytes);
    deflater.finish();
    deflater.deflate(compressedData);
    harness.check(true);
  }
}
