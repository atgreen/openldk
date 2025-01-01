/* DragSelectTest.java -- Tests if drag selection works
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
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

// Tags: JDK1.2 manual

package gnu.testlet.javax.swing.JMenuItem;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JRootPane;

import gnu.testlet.VisualTestlet;

public class DragSelectTest extends VisualTestlet
{
  public String getInstructions()
  {
    return "Press the mouse on the 'Menu' menu, hold the button pressed and "
           + "drag it to one of the menu items. Then release the mouse "
           + "button";
  }

  public String getExpectedResults()
  {
    return "The menu should be closed and the name of the menu item shown in "
           + "the panel below";
  }

  public Component getTestComponent()
  {
    JRootPane rp = new JRootPane();
    JMenuBar mb = new JMenuBar();
    JMenu menu = new JMenu("Menu");
    final JLabel label =
      new JLabel("The selected menu item should show up here");
    ActionListener l = new ActionListener()
    {
      public void actionPerformed(ActionEvent ev)
      {
        JMenuItem i = (JMenuItem) ev.getSource();
        label.setText(i.getText());
      }
    };
    
    JMenuItem item1 = new JMenuItem("MenuItem 1");
    item1.addActionListener(l);
    JMenuItem item2 = new JMenuItem("MenuItem 2");
    item2.addActionListener(l);
    JMenuItem item3 = new JMenuItem("MenuItem 3");
    item3.addActionListener(l);
    menu.add(item1);
    menu.add(item2);
    menu.add(item3);
    mb.add(menu);
    rp.setJMenuBar(mb);
    rp.getContentPane().add(label);
    return rp;
  }

}
