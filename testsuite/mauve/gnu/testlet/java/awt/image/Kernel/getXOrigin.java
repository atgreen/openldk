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
 * Some checks for the getXOrigin() method in the {@link Kernel} class.
 */
public class getXOrigin implements Testlet 
{

  /**
   * Runs the test using the specified harness.  
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)       
  {
    Kernel k1 = new Kernel(3, 4, new float[] {1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f, 
            9f, 10f, 11f, 12f});
    harness.check(k1.getXOrigin(), 1);
    Kernel k2 = new Kernel(4, 3, new float[] {1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f, 
            9f, 10f, 11f, 12f});
    harness.check(k2.getXOrigin(), 1);
    Kernel k3 = new Kernel(5, 2, new float[] {1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f, 
            9f, 10f});
    harness.check(k3.getXOrigin(), 2);
  }

}

