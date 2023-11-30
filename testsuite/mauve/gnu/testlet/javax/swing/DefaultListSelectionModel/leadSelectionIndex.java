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

package gnu.testlet.javax.swing.DefaultListSelectionModel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import javax.swing.JList;
import javax.swing.DefaultListModel;
import javax.swing.ListSelectionModel;

/**
 * Tests if DefaultListSelectionModel.setLeadSelectionIndex checks the selection
 * mode to see if it should extend the selection or set the selection.
 */ 
public class leadSelectionIndex implements Testlet {
	
	public void test(TestHarness harness) {
          DefaultListModel v = new DefaultListModel();
          v.addElement("0");
          v.addElement("1");
          v.addElement("2");
          v.addElement("3");
          v.addElement("4");
          JList a = new JList(v);
          a.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
          a.setSelectedIndex(1);
          a.getSelectionModel().setLeadSelectionIndex(3);
          if (!(a.getSelectedIndices().length == 1 && a.getSelectedIndices()[0] == 3))
            harness.fail("setLeadSelectionIndex should check the selection mode");
	}

}
