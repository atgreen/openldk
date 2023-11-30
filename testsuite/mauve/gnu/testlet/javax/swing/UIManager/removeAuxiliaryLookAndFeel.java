// Tags: JDK1.4
// Uses: MyLookAndFeel

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

package gnu.testlet.javax.swing.UIManager;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.LookAndFeel;
import javax.swing.UIManager;

/**
 * Tests the removeAuxiliaryLookAndFeel() method in the {@link UIManager} class.
 */
public class removeAuxiliaryLookAndFeel implements Testlet {
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    test1(harness);
    test2(harness);
  }
  
  private void test1(TestHarness harness) 
  {
    LookAndFeel laf = new MyLookAndFeel();
    harness.check(UIManager.getAuxiliaryLookAndFeels(), null);
    UIManager.addAuxiliaryLookAndFeel(laf);
    LookAndFeel[] auxLafs = UIManager.getAuxiliaryLookAndFeels();
    harness.check(auxLafs.length, 1);
    harness.check(auxLafs[0], laf);
    boolean b = UIManager.removeAuxiliaryLookAndFeel(laf);  
    harness.check(b, true);
    harness.check(UIManager.getAuxiliaryLookAndFeels(), null);
    
    // try removing a null LAF
    boolean pass = true;
    try
    {
      b = UIManager.removeAuxiliaryLookAndFeel(null);
    }
    catch (NullPointerException e)
    {
      pass = false;
    }
    harness.check(pass);
    harness.check(b, false);
  }

  /**
   * Here we check that removing a LAF preserves the order of the remaining 
   * LAFs.
   * 
   * @param harness
   */
  private void test2(TestHarness harness) 
  {
    harness.checkPoint("test2");
    // first check that we are starting with 0 auxiliary LAFs
    harness.check(UIManager.getAuxiliaryLookAndFeels(), null);
    
    LookAndFeel laf1 = new MyLookAndFeel();
    LookAndFeel laf2 = new MyLookAndFeel();
    LookAndFeel laf3 = new MyLookAndFeel();
    LookAndFeel laf4 = new MyLookAndFeel();
    
    UIManager.addAuxiliaryLookAndFeel(laf1);
    UIManager.addAuxiliaryLookAndFeel(laf2);
    UIManager.addAuxiliaryLookAndFeel(laf3);
    UIManager.addAuxiliaryLookAndFeel(laf4);
    
    UIManager.removeAuxiliaryLookAndFeel(laf2);
    LookAndFeel[] lafs = UIManager.getAuxiliaryLookAndFeels();
    harness.check(lafs[0], laf1);
    harness.check(lafs[1], laf3);
    harness.check(lafs[2], laf4);
    
    UIManager.removeAuxiliaryLookAndFeel(laf1);
    lafs = UIManager.getAuxiliaryLookAndFeels();    
    harness.check(lafs[0], laf3);
    harness.check(lafs[1], laf4);

    UIManager.removeAuxiliaryLookAndFeel(laf4);
    lafs = UIManager.getAuxiliaryLookAndFeels();    
    harness.check(lafs[0], laf3);
    
    UIManager.removeAuxiliaryLookAndFeel(laf3);
    lafs = UIManager.getAuxiliaryLookAndFeels();    
    harness.check(lafs, null);
  }
}
