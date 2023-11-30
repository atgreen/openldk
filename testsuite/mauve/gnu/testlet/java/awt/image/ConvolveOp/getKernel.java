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

package gnu.testlet.java.awt.image.ConvolveOp ;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.ConvolveOp;
import java.awt.image.Kernel;
import java.util.Arrays;

/**
 * Some checks for the getKernel() method in the {@link ConvolveOp} class.
 */
public class getKernel implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    Kernel k1 = new Kernel(1, 1, new float[] {1f});
    ConvolveOp op1 = new ConvolveOp(k1);
    Kernel k2 = op1.getKernel();
    harness.check(Arrays.equals(k1.getKernelData(null), k2.getKernelData(null)));
    harness.check(k1 != k2);
    
    // each call seems to clone the kernel
    Kernel k3 = op1.getKernel();
    harness.check(k2 != k3);
  }

}

