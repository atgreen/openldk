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
 * Some checks for the <code>getPixel()()</code> method in the 
 * {@link BandedSampleModel} class.
 */
public class getPixel implements Testlet 
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
    b.setElem(0, 0, 0xA0);
    b.setElem(0, 1, 0xA1);
    b.setElem(0, 2, 0xA2);
    b.setElem(0, 3, 0xA3);
    b.setElem(0, 4, 0xA4);
    b.setElem(0, 5, 0xA5);
    b.setElem(1, 0, 0xB0);
    b.setElem(1, 1, 0xB1);
    b.setElem(1, 2, 0xB2);
    b.setElem(1, 3, 0xB3);
    b.setElem(1, 4, 0xB4);
    b.setElem(1, 5, 0xB5);
    harness.check(m.getPixel(0, 0, (int[]) null, b)[0], 0xA0);
    harness.check(m.getPixel(1, 0, (int[]) null, b)[0], 0xA1);
    harness.check(m.getPixel(0, 1, (int[]) null, b)[0], 0xA2);
    harness.check(m.getPixel(1, 1, (int[]) null, b)[0], 0xA3);
    harness.check(m.getPixel(0, 2, (int[]) null, b)[0], 0xA4);
    harness.check(m.getPixel(1, 2, (int[]) null, b)[0], 0xA5);
    harness.check(m.getPixel(0, 0, (int[]) null, b)[1], 0xB0);
    harness.check(m.getPixel(1, 0, (int[]) null, b)[1], 0xB1);
    harness.check(m.getPixel(0, 1, (int[]) null, b)[1], 0xB2);
    harness.check(m.getPixel(1, 1, (int[]) null, b)[1], 0xB3);
    harness.check(m.getPixel(0, 2, (int[]) null, b)[1], 0xB4);
    harness.check(m.getPixel(1, 2, (int[]) null, b)[1], 0xB5);
 
    // try negative x
    boolean pass = false;
    try
    {
      /* int[] pixel = */ m.getPixel(-1, 0, (int[]) null, b);   
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;   
    }
    harness.check(pass);

    // try negative y
    pass = false;
    try
    {
      /* int[] pixel = */ m.getPixel(0, -1, (int[]) null, b);   
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;   
    }
    harness.check(pass);

    // try pixel array too small
    pass = false;
    try 
    {
      /* int[] pixel = */ m.getPixel(0, 0, new int[1], b);   
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try null buffer
    pass = false;
    try 
    {
      /* int[] pixel = */ m.getPixel(0, 0, (int[]) null, null);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
  }

}

