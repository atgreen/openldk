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
 * Some checks for the <code>getSampleDouble()()</code> method in the 
 * {@link BandedSampleModel} class.
 */
public class getSampleDouble implements Testlet 
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
    b.setElem(0, 0, 0xFFA0);
    b.setElem(0, 1, 0xFFA1);
    b.setElem(0, 2, 0xFFA2);
    b.setElem(0, 3, 0xFFA3);
    b.setElem(0, 4, 0xFFA4);
    b.setElem(0, 5, 0xFFA5);
    b.setElem(1, 0, 0xFFB0);
    b.setElem(1, 1, 0xFFB1);
    b.setElem(1, 2, 0xFFB2);
    b.setElem(1, 3, 0xFFB3);
    b.setElem(1, 4, 0xFFB4);
    b.setElem(1, 5, 0xFFB5);
    harness.check(m.getSampleDouble(0, 0, 0, b), 0xFFA0);
    harness.check(m.getSampleDouble(1, 0, 0, b), 0xFFA1);
    harness.check(m.getSampleDouble(0, 1, 0, b), 0xFFA2);
    harness.check(m.getSampleDouble(1, 1, 0, b), 0xFFA3);
    harness.check(m.getSampleDouble(0, 2, 0, b), 0xFFA4);
    harness.check(m.getSampleDouble(1, 2, 0, b), 0xFFA5);
    harness.check(m.getSampleDouble(0, 0, 1, b), 0xFFB0);
    harness.check(m.getSampleDouble(1, 0, 1, b), 0xFFB1);
    harness.check(m.getSampleDouble(0, 1, 1, b), 0xFFB2);
    harness.check(m.getSampleDouble(1, 1, 1, b), 0xFFB3);
    harness.check(m.getSampleDouble(0, 2, 1, b), 0xFFB4);
    harness.check(m.getSampleDouble(1, 2, 1, b), 0xFFB5);

    // try negative x
    boolean pass = false;
    try  
    {
      /* int sample = */ m.getSampleDouble(-1, 0, 0, b);   
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
      /* int sample = */ m.getSampleDouble(0, -1, 0, b);   
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;   
    }
    harness.check(pass);

    // try negative band
    pass = false;
    try
    {
      /* int sample = */ m.getSampleDouble(0, 0, -1, b);   
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
      /* int sample = */ m.getSampleDouble(0, 0, 0, null);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
  }

}

