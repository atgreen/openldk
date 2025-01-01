// Tags: JDK1.2

// Copyright (C) 2006 Mark J. Wielaard (mark@klomp.org)

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


package gnu.testlet.javax.swing.DefaultListCellRenderer;

import java.awt.*;
import javax.swing.*;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getListCellRendererComponent implements Testlet
{

  public void test(TestHarness harness)
  {
    Object[] data = new Object[] { "", null, new Integer(1), new Object(),
				   new int[0], new String[0], new String[1] };
    JList list = new JList(data);
    DefaultListCellRenderer dlcr = new DefaultListCellRenderer();
    for (int i = 0; i < data.length; i++)
      {
	Component c = dlcr.getListCellRendererComponent(list, data[i],
							i, false, false);
	Dimension d = c.getPreferredSize();
	harness.check(d.height >= 0);
	harness.check(d.width >= 0);
      }

    for (int i = 0; i < data.length; i++)
      {
	list.setSelectedIndex(i);
	Component c = dlcr.getListCellRendererComponent(list, data[i],
							i, true, false);
	Dimension d = c.getPreferredSize();
	harness.check(d.height >= 0);
	harness.check(d.width >= 0);
      }
  }
}
