// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.awt.image.BandedSampleModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.BandedSampleModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferInt;

/**
 * Some checks for the <code>setSamples()</code> method in the 
 * {@link BandedSampleModel} class.
 */
public class setSamples implements Testlet 
{
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)        
  {
    BandedSampleModel m = new BandedSampleModel(DataBuffer.TYPE_INT, 2, 3, 2);
    DataBufferInt b = new DataBufferInt(6, 2);
    m.setSamples(0, 0, 1, 3, 0, new int[] {0xA0, 0xA2, 0xA4}, b);
    m.setSamples(1, 0, 1, 1, 0, new int[] {0xA1}, b);
    m.setSamples(1, 1, 1, 2, 0, new int[] {0xA3, 0xA5}, b);
    m.setSamples(0, 0, 1, 3, 1, new int[] {0xB0, 0xB2, 0xB4}, b);
    m.setSamples(1, 0, 1, 3, 1, new int[] {0xB1, 0xB3, 0xB5}, b);
    harness.check(b.getElem(0, 0), 0xA0);  // check 1
    harness.check(b.getElem(0, 1), 0xA1);  // check 2
    harness.check(b.getElem(0, 2), 0xA2);  // check 3
    harness.check(b.getElem(0, 3), 0xA3);  // check 4
    harness.check(b.getElem(0, 4), 0xA4);  // check 5
    harness.check(b.getElem(0, 5), 0xA5);  // check 6
    harness.check(b.getElem(1, 0), 0xB0);  // check 7
    harness.check(b.getElem(1, 1), 0xB1);  // check 8
    harness.check(b.getElem(1, 2), 0xB2);  // check 9
    harness.check(b.getElem(1, 3), 0xB3);  // check 10
    harness.check(b.getElem(1, 4), 0xB4);  // check 11
    harness.check(b.getElem(1, 5), 0xB5);  // check 12

    // check negative x
    boolean pass = false;
    try
    {
      m.setSamples(-1, 0, 0, 3, 0, new int[] {0xA0, 0xA2, 0xA4}, b);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;   
    }
    harness.check(pass);                   // check 13

    // check negative y
    pass = false;
    try
    {
      m.setSamples(0, -1, 0, 3, 0, new int[] {0xA0, 0xA2, 0xA4}, b);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;   
    }
    harness.check(pass);                   // check 14

    // check negative band
    pass = false;
    try
    {
      m.setSamples(0, 0, 1, 3, -1, new int[] {0xA0, 0xA2, 0xA4}, b);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // check sample array too short
    pass = false;
    try
    {
      m.setSamples(0, 0, 1, 3, -1, new int[] {0xA0, 0xA2, /*0xA4*/}, b);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // check null sample array
    pass = false;
    try
    {
      m.setSamples(0, 0, 1, 3, 0, (int[]) null, b);
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // check null buffer
    pass = false;
    try
    {
      m.setSamples(0, 0, 1, 3, 0, new int[] {0xA0, 0xA2, 0xA4}, null);
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  }

}

