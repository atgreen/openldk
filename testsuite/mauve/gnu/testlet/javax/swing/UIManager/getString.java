/* getString.java -- some checks for the getString() methods in the 
     UIManager class.
   Copyright (C) 2006 David Gilbert  <david.gilbert@object-refinery.com>
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

// Tags: JDK1.4

package gnu.testlet.javax.swing.UIManager;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Locale;

import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.plaf.metal.MetalLookAndFeel;

public class getString implements Testlet
{
  public void test(TestHarness harness) 
  {
    try
    {
      UIManager.setLookAndFeel(new MetalLookAndFeel());
    }
    catch (UnsupportedLookAndFeelException e) 
    {
      e.printStackTrace();
    }
    test1(harness);
    test2(harness);
  }
  
  public void test1(TestHarness harness) 
  {
    harness.checkPoint("(Object)");
    harness.check(UIManager.getString("OptionPane.errorSound"), 
            "sounds/OptionPaneError.wav");
    UIManager.put("OptionPane.errorSound", "ABC");
    harness.check(UIManager.getString("OptionPane.errorSound"), 
            "ABC");
    UIManager.put("OptionPane.errorSound", null);
    harness.check(UIManager.getString("OptionPane.errorSound"), 
            "sounds/OptionPaneError.wav");
    
    // check an item that is not a String - it should return null
    harness.check(UIManager.getString("ScrollBar.darkShadow"), null);
    
    // check an item that doesn't exist - it should return null
    harness.check(UIManager.getString("XXXXXXXXXXXXXXXXX"), null);
    
    // try null
    boolean pass = false;
    try 
    {
      UIManager.getString(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  public void test2(TestHarness harness) 
  {
    harness.checkPoint("(Object, Locale)");    

    // try null
    boolean pass = false;
    try 
    {
      UIManager.getString(null, Locale.getDefault());
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
