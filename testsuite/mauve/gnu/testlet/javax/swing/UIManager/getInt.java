/* getInt.java -- some checks for the getInt() methods in the 
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

public class getInt implements Testlet
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
    harness.check(UIManager.getInt("Button.textIconGap"), 4);
    UIManager.put("Button.textIconGap", new Integer(999));
    harness.check(UIManager.getInt("Button.textIconGap"), 999);
    UIManager.put("Button.textIconGap", null);
    harness.check(UIManager.getInt("Button.textIconGap"), 4);
    
    // check an item that is not a boolean - it should return false
    harness.check(UIManager.getInt("ScrollBar.darkShadow"), 0);
    
    // check an item that doesn't exist - it should return false
    harness.check(UIManager.getInt("XXXXXXXXXXXXXXXXX"), 0);
    
    // try null
    boolean pass = false;
    try 
    {
      UIManager.getInt(null);
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
      UIManager.getInt(null, Locale.getDefault());
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
