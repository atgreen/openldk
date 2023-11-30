// Tags: JDK1.2

// Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details. 

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301 USA.

package gnu.testlet.java.awt.image.Kernel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.Kernel;

/**
 * Some checks for the constructor in the {@link Kernel} class.
*/
public class constructor implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    float[] d1 = new float[] {1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f, 
            9f, 10f, 11f, 12f, 13f, 14f, 15f};
    Kernel k1 = new Kernel(3, 5, d1);
    harness.check(k1.getWidth(), 3);
    harness.check(k1.getHeight(), 5);
    harness.check(k1.getXOrigin(), 1);
    harness.check(k1.getYOrigin(), 2);
    
    // changing source data array shouldn't change the kernel
    d1[0] = Float.NaN;
    float[] d2 = k1.getKernelData(null);
    harness.check(!Float.isNaN(d2[0]));
    
    // try negative width - the required behaviour is not specified, the 
    // reference implementation throws a NegativeArraySizeException, but that 
    // seems like an implementation detail so this test is going to allow an
    // IllegalArgumentException as well (which is what GNU Classpath does).
    boolean pass = false;
    try
    {
      k1 = new Kernel(-1, 2, new float[] {1f, 2f});
    }
    catch (NegativeArraySizeException e)
    {
      pass = true;   
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try negative height - the required behaviour is not specified, the 
    // reference implementation throws a NegativeArraySizeException, but that 
    // seems like an implementation detail so this test is going to allow an
    // IllegalArgumentException as well (which is what GNU Classpath does).
    pass = false;
    try
    {
      k1 = new Kernel(1, -2, new float[] {1f, 2f});
    }
    catch (NegativeArraySizeException e)
    {
      pass = true;   
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try null array
    pass = false;
    try
    {
      k1 = new Kernel(1, 2, null);
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
  }

}

