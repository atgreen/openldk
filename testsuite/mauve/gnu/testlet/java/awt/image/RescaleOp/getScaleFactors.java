/* getScaleFactors.java -- some checks for the getScaleFactors() method in the
       RescaleOp class.
   Copyright (C) 2006 Francis Kung <fkung@redhat.com>
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

// Tags: JDK1.4

package gnu.testlet.java.awt.image.RescaleOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.RescaleOp;
import java.util.Arrays;

public class getScaleFactors implements Testlet
{
  public void test(TestHarness harness)
  {
    RescaleOp op = new RescaleOp(1, 1, null);
    harness.check(Arrays.equals(op.getScaleFactors(null), new float[]{1}));
    
    op = new RescaleOp(new float[]{1}, new float[]{1}, null);
    harness.check(Arrays.equals(op.getScaleFactors(null), new float[]{1}));
    
    float[] flt = new float[]{1, 2, 3, 4, 5};
    op = new RescaleOp(flt, flt, null);
    harness.check(op.getScaleFactors(null) != flt);
    harness.check(Arrays.equals(op.getScaleFactors(null),
                                new float[]{1, 2, 3, 4, 5}));
    
    // Mismatched array sizes...
    op = new RescaleOp(flt, new float[]{1, 2}, null);
    harness.check(op.getScaleFactors(null).length == 2);
    harness.check(op.getScaleFactors(null)[0] == 1);
    harness.check(op.getScaleFactors(null)[1] == 2);
    
    // Try passing in an array to be populated
    op = new RescaleOp(flt, flt, null);
    float[] arr = new float[5];
    harness.check(Arrays.equals(op.getScaleFactors(arr), arr));
    harness.check(Arrays.equals(arr, flt));
    
    // What if the array is too big?
    arr = new float[10];
    Arrays.fill(arr, 25);
    op.getScaleFactors(arr);
    for (int i = 0; i < 5; i++)
      harness.check(arr[i] == flt[i]);
    for (int i = 5; i < arr.length; i++)
      harness.check(arr[i] == 25);
    
    // What if array is too small?
    arr = new float[2];
    try
    {
      harness.check(op.getScaleFactors(arr).length == 2);
      harness.check(op.getScaleFactors(arr)[0] == 1);
      harness.check(op.getScaleFactors(arr)[1] == 2);
    }
    catch (ArrayIndexOutOfBoundsException ex)
    {
      harness.check(false);
    }
  }
}
