/* getSamples.java -- some checks for the getSamples() method in the 
       ComponentSampleModel class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

package gnu.testlet.java.awt.image.ComponentSampleModel;

import java.awt.image.ComponentSampleModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferInt;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getSamples implements Testlet
{
  public void test(TestHarness harness)
  {
    DataBuffer db = new DataBufferInt(12);
    for (int i = 0; i < 12; i++)
      db.setElem(i, i);
    ComponentSampleModel m = new ComponentSampleModel(DataBuffer.TYPE_INT,
            3, 2, 2, 6, new int[] {0, 1});
    int[] samples = m.getSamples(0, 1, 2, 1, 0, (int[]) null, db);
    harness.check(samples.length, 2);
    harness.check(samples[0], 6);
    harness.check(samples[1], 8);
    
    // try passing in a result array
    int[] result = new int[2];
    samples = m.getSamples(0, 1, 2, 1, 1, result, db);
    harness.check(samples.length, 2);
    harness.check(samples[0], 7);
    harness.check(samples[1], 9);
    harness.check(samples == result);
    
    // try null data buffer
    boolean pass = false;
    try
      {
        m.getSamples(0, 1, 2, 1, 1, result, null);
      }
    catch (NullPointerException e)
      {
        pass = true;  
      }
    harness.check(pass);
  }
}
