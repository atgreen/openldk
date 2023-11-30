// Tags: JDK1.2

// Copyright (C) 2005 Red Hat.

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

package gnu.testlet.java.awt.BorderLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.LayoutManager2;

import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 * Some checks for the maximumLayoutSize() method in the 
 * {@link BorderLayout} class.  
 */
public class maxLayoutSize implements Testlet 
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)    
  {
    JTextField ftf = new JTextField("HELLO WORLD");
    JPanel borderPanel = new JPanel(new java.awt.BorderLayout());
    borderPanel.add(ftf);
    LayoutManager2 lm = (LayoutManager2) borderPanel.getLayout();
    Dimension max = new Dimension (Integer.MAX_VALUE, Integer.MAX_VALUE);

    // Check that the layout manager returns Integer.MAX_VALUE for both 
    // max dimensions, regardless of whether or not there's a border
    harness.check (lm.maximumLayoutSize(borderPanel), max);
    borderPanel.setBorder(new javax.swing.border.TitledBorder("HELLO WORLD"));    
    harness.check (lm.maximumLayoutSize(borderPanel), max);

    // Check that maximumLayoutSize isn't affected by the layout size of 
    // the contained components
    ftf.setMaximumSize(new Dimension (0,0));
    harness.check (lm.maximumLayoutSize(borderPanel), max);

    // Check that maximumLayoutSize returns Integer.MAX_VALUE even for null
    // arguments
    harness.check (lm.maximumLayoutSize(null), max);

    // Check that a brand new BorderLayout unassociated with any Component
    // also returns Integer.MAX_VALUE for a null argument.
    BorderLayout bl = new BorderLayout();
    harness.check (bl.maximumLayoutSize(null), max);
  }
}
