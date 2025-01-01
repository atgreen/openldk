// Tags: JDK1.2

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.awt.image.SinglePixelPackedSampleModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.DataBuffer;
import java.awt.image.SinglePixelPackedSampleModel;

/**
 * Some checks for the <code>equals()</code> method in the 
 * {@link SinglePixelPackedSampleModel} class.
 */
public class equals implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    SinglePixelPackedSampleModel m1 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_BYTE, 1, 2, new int[] { 224, 28, 3 }
    );
    SinglePixelPackedSampleModel m2 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_BYTE, 1, 2, new int[] { 224, 28, 3 }
    );
    harness.check(m1.equals(m2));              // check 1
    harness.check(m2.equals(m1));              // check 2
    harness.check(!m1.equals(null));           // check 3
    
    // check that all fields are distinguished...
    
    // dataType
    m1 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_INT, 1, 2, new int[] { 224, 28, 3 }
    );
    harness.check(!m1.equals(m2));
    m2 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_INT, 1, 2, new int[] { 224, 28, 3 }
    );
    harness.check(m1.equals(m2));

    // w
    m1 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_INT, 5, 2, new int[] { 224, 28, 3 }
    );
    harness.check(!m1.equals(m2));
    m2 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_INT, 5, 2, new int[] { 224, 28, 3 }
    );
    harness.check(m1.equals(m2));
  
    // h
    m1 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_INT, 5, 10, new int[] { 224, 28, 3 }
    );
    harness.check(!m1.equals(m2));
    m2 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_INT, 5, 10, new int[] { 224, 28, 3 }
    );
    harness.check(m1.equals(m2));

    // bitmasks
    m1 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_INT, 5, 10, new int[] { 224, 24, 7 }
    );
    harness.check(!m1.equals(m2));
    m2 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_INT, 5, 10, new int[] { 224, 24, 7 }
    );
    harness.check(m1.equals(m2));

    // scanline stride
    m1 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_INT, 5, 10, 99, new int[] {224, 24, 7 }
    );
    harness.check(!m1.equals(m2));
    m2 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_INT, 5, 10, 99, new int[] {224, 24, 7 }
    );
    harness.check(m1.equals(m2));  
  }
  
}

