// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

// This file is part of Mauve.

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.plaf.metal.MetalComboBoxButton;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Insets;

import javax.swing.CellRendererPane;
import javax.swing.Icon;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.metal.MetalComboBoxButton;
import javax.swing.plaf.metal.MetalComboBoxIcon;

/**
 * Some tests for the constructors in the {@link MetalComboBoxButton} class.
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

  private void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("MetalComboBoxButton(JComboBox, Icon, CellRendererPane, JList)");        
    JComboBox jcb = new JComboBox(new Object[] {"A", "B", "C"});
    Icon icon = new MetalComboBoxIcon();
    MetalComboBoxButton b = new MetalComboBoxButton(jcb, icon,
            new CellRendererPane(), new JList());
    harness.check(b.getComboBox() == jcb);
    harness.check(b.getComboIcon() == icon);
    harness.check(!b.isIconOnly());
    Insets margin = b.getMargin();
    harness.check(margin, new Insets(2, 14, 2, 14));
    harness.check(margin instanceof UIResource);
    Insets insets = b.getInsets();
    harness.check(insets, new Insets(5, 17, 5, 17));
    
    boolean pass = false;
    try
    {
      b = new MetalComboBoxButton(null, icon, new CellRendererPane(), 
              new JList());
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    b = new MetalComboBoxButton(jcb, null, new CellRendererPane(), new JList());
    harness.check(b.getComboIcon() == null);
    b = new MetalComboBoxButton(jcb, icon, null, new JList());
    b = new MetalComboBoxButton(jcb, icon, new CellRendererPane(), null);
  }
  
  private void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("MetalComboBoxButton(JComboBox, Icon, boolean, CellRendererPane, JList)");        
    JComboBox jcb = new JComboBox(new Object[] {"A", "B", "C"});
    Icon icon = new MetalComboBoxIcon();
    MetalComboBoxButton b = new MetalComboBoxButton(jcb, icon, true, 
            new CellRendererPane(), new JList());
    harness.check(b.getComboBox() == jcb);
    harness.check(b.getComboIcon() == icon);
    harness.check(b.isIconOnly());
    
    boolean pass = false;
    try
    {
      b = new MetalComboBoxButton(null, icon, true, new CellRendererPane(), 
            new JList());
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    b = new MetalComboBoxButton(jcb, null, true, new CellRendererPane(), 
            new JList());
    harness.check(b.getComboIcon() == null);
    
    b = new MetalComboBoxButton(jcb, icon, true, null, new JList());
    b = new MetalComboBoxButton(jcb, icon, true, new CellRendererPane(), null);
  }

}
