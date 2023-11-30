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
 * Some checks for the constructors in the {@link ConvolveOp} class.
 */
public class constructors implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    testConstructor1(harness);
    testConstructor2(harness);
   }

  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("(Kernel)");
    Kernel k1 = new Kernel(3, 3, new float[] {1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f, 9f});
    ConvolveOp op1 = new ConvolveOp(k1);
    harness.check(op1.getKernel() != k1);
    harness.check(Arrays.equals(op1.getKernel().getKernelData(null), k1.getKernelData(null)));
    harness.check(op1.getEdgeCondition(), ConvolveOp.EDGE_ZERO_FILL);
    harness.check(op1.getRenderingHints(), null);
    
    // using a null Kernel doesn't throw an immediate exception...
    boolean pass = true;
    try
    {
      /*ConvolveOp op2 =*/ new ConvolveOp(null);    
    }
    catch (NullPointerException e) 
    {
      pass = false;   
    }
    harness.check(pass);
    
    // ...but it does later
    pass = false;
    try
    {
      ConvolveOp op2 = new ConvolveOp(null);    
      /*Kernel k2 =*/ op2.getKernel();   
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  }

  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(Kernel, int, RenderingHints)");
    Kernel k1 = new Kernel(3, 3, new float[] {1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f, 9f});
    ConvolveOp op1 = new ConvolveOp(k1, ConvolveOp.EDGE_NO_OP, null);
    harness.check(op1.getKernel() != k1);
    harness.check(Arrays.equals(op1.getKernel().getKernelData(null), k1.getKernelData(null)));
    harness.check(op1.getEdgeCondition(), ConvolveOp.EDGE_NO_OP);
    harness.check(op1.getRenderingHints(), null);
    
    // a null kernel will fail when you try to access it
    boolean pass = false;
    ConvolveOp op2 = new ConvolveOp(null, ConvolveOp.EDGE_NO_OP, null);
    try
    {
      /*Kernel k =*/ op2.getKernel();   
    }
    catch (NullPointerException e) 
    { 
      pass = true;   
    }
    harness.check(pass);
    
    // try an invalid edge operation code
    pass = false;
    op1 = new ConvolveOp(k1, -1, null);
    harness.check(op1.getEdgeCondition(), -1);
  }
  
}

