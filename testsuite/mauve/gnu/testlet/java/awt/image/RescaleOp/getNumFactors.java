/* getNumFactors.java -- some checks for the getNumFactors() method of the
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

// Tags: JDK1.2

package gnu.testlet.java.awt.image.RescaleOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.RescaleOp;

/**
 * Checks the getNumFactors method in the
 * {@link RescaleOp} class.
 */
public class getNumFactors implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("getNumFactors");

    RescaleOp op = new RescaleOp(1.5f, 2.3f, null);
    harness.check(op.getNumFactors(), 1);
    
    op = new RescaleOp(new float[]{1.5f}, new float[]{2.3f}, null);
    harness.check(op.getNumFactors(), 1);
   
    op = new RescaleOp(new float[]{1.5f, 2.5f}, new float[]{2.3f, 6.6f}, null);
    harness.check(op.getNumFactors(), 2);
    
    op = new RescaleOp(new float[]{1.5f, 2.2f, 3.7f},
                       new float[]{2.3f, 2.3f, 2.3f},
                       null);
    harness.check(op.getNumFactors(), 3);
    
    op = new RescaleOp(new float[]{}, new float[]{}, null);
    harness.check(op.getNumFactors(), 0);
    
    // If the arrays are mismatched, return the lower value
    op = new RescaleOp(new float[]{1, 2, 3}, new float[]{1}, null);
    harness.check(op.getNumFactors(), 1);
    op = new RescaleOp(new float[]{1}, new float[]{1, 2, 3}, null);
    harness.check(op.getNumFactors(), 1);
    op = new RescaleOp(new float[]{1, 2}, new float[]{1, 2, 3}, null);
    harness.check(op.getNumFactors(), 2);
    op = new RescaleOp(new float[]{1, 2, 3}, new float[]{1, 2}, null);
    harness.check(op.getNumFactors(), 2);
  }
}

