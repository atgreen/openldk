//Tags: JDK1.2

//Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.

package gnu.testlet.java.awt.image.DataBufferUShort;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.DataBufferUShort;
import java.util.Arrays;

/**
 * Some tests for the getBankData() method in the {@link DataBufferUShort} class.
 */
public class getBankData implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    // check that array updates pass through
    short[][] data = new short[][] {{1, 2}};
    DataBufferUShort b = new DataBufferUShort(data, 2);
    short[][] banks = b.getBankData();
    harness.check(Arrays.equals(b.getBankData(), data));
    data[0][0] = 3;
    harness.check(banks[0][0] == 3);

    // test where supplied array is bigger than 'size'
    data = new short[][] {{1, 2, 3}};
    b = new DataBufferUShort(data, 2);
    banks = b.getBankData();
    harness.check(Arrays.equals(b.getBankData(), data));

    // test where offsets are specified
    data = new short[][] {{1, 2, 3}, {4, 5, 6, 7}};
    b = new DataBufferUShort(data, 2, new int[] {0, 1});
    banks = b.getBankData();
    harness.check(Arrays.equals(b.getBankData(), data));

    // check that a single bank buffer returns a valid array
    DataBufferUShort b2 = new DataBufferUShort(new short[] {1, 2}, 2);
    banks = b2.getBankData();
    harness.check(banks.length == 1);
    harness.check(banks[0][0] == 1);
    harness.check(banks[0][1] == 2);

    // check that a multi bank buffer returns a valid array
    DataBufferUShort b3 = new DataBufferUShort(new short[][] {{1}, {2}}, 1);
    banks = b3.getBankData();
    harness.check(banks.length == 2);
    harness.check(banks[0][0] == 1);
    harness.check(banks[1][0] == 2);
  }

}
