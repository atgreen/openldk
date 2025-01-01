// Tags: JDK1.4

// Copyright (C) 2004 Audrius Meskauskas <audriusa@bluewin.ch>

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.JComboBox;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.ActionListener;
import java.awt.event.ItemListener;

import javax.swing.JComboBox;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;

/**
 * The basic JComboBox listener array getters (feature of 1.4).
 * This test must also work when the component is not displayed.
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class listenerList
  implements Testlet, ActionListener, ItemListener
{
  public void test(TestHarness harness)
  {
    JComboBox box = new JComboBox();
    box.addActionListener(this);
    box.addItemListener(this);
    checkListenerLists(harness, box);
  }

  private void checkListenerLists(TestHarness harness, JComboBox box)
  {
    try
      {
        ActionListener a_listeners[] = box.getActionListeners();
        boolean weAre = false;
        for (int i = 0; i < a_listeners.length; i++)
          {
            if (a_listeners [ i ] == this)
              {
                weAre = true;
                break;
              }
          }
        harness.check(weAre, "Action listener list");

        ItemListener i_listeners[] = box.getItemListeners();
        weAre = false;
        for (int i = 0; i < i_listeners.length; i++)
          {
            if (i_listeners [ i ] == this)
              {
                weAre = true;
                break;
              }
          }
        harness.check(weAre, "Item listener list");
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        harness.fail("Cannot check the listerner lists, bug in 1.3, fixed in 1.4");
      }
  }

  public void actionPerformed(ActionEvent e)
  {
  }

  public void itemStateChanged(ItemEvent ie)
  {
  }

}
