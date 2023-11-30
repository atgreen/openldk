//Tags: JDK1.2

//Copyright (C) 2005 Red Hat.

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.JPanel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.Dimension;

import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 * This tests whether setting the border on a JPanel affects its maximum size.
 */ 
public class setBorder implements Testlet 
{
  
  public void test(TestHarness harness) 
  {
    JTextField ftf = new JTextField("HELLO WORLD");
    JPanel borderPanel = new JPanel(new java.awt.BorderLayout());
    borderPanel.add(ftf);

    // Show that setting the border messes up the maximumSize
    Dimension d1 = borderPanel.getMaximumSize();
    borderPanel.setBorder(new javax.swing.border.TitledBorder("HELLO WORLD"));    
    Dimension d2 = borderPanel.getMaximumSize();
    harness.check(d1,d2);
  }
}
