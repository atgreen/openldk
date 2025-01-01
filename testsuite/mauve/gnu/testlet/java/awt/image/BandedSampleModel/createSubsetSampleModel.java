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
import java.awt.image.RasterFormatException;

/**
 * Some checks for the <code>createSubsetSampleModel()</code> method in the 
 * {@link BandedSampleModel} class.
 */
public class createSubsetSampleModel implements Testlet 
{
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)        
  {
    BandedSampleModel m1 = new BandedSampleModel(DataBuffer.TYPE_SHORT, 10, 20, 
      10, new int[] {2, 1, 0}, new int[] {4, 5, 6});
    BandedSampleModel m2 = (BandedSampleModel) m1.createSubsetSampleModel(new int[] {0, 1});
    harness.check(m2.getDataType(), DataBuffer.TYPE_SHORT);
    harness.check(m2.getWidth(), 10);
    harness.check(m2.getHeight(), 20);
    harness.check(m2.getNumBands(), 2);
    harness.check(m2.getBandOffsets()[0], 4);
    harness.check(m2.getBandOffsets()[1], 5);   
    
    // try more bands than available
    boolean pass = false;
    try
    {
      m2 = (BandedSampleModel) m1.createSubsetSampleModel(new int[] {0, 1, 2, 3}); 
    }
    catch (RasterFormatException e)
    {
      pass = true;   
    }
    harness.check(pass);
  }

}

