// Tags: JDK1.2
// Uses: TestModel2

//Copyright (C) 2004 Robert Schuster <thebohemian@gmx.net>

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

package gnu.testlet.javax.swing.JComboBox;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JComboBox;

/** This tests the model modifying methods of JComboBox. The model
 * being used is a valid MutableComboBoxModel. It contains 10 elements
 * at the beginning and shifts its items because it is based on a linked
 * list.
 * 
 * @author Robert Schuster
 */
public class MutableTest2 implements Testlet
{

    public void test(TestHarness harness)
    {
        JComboBox combo = new JComboBox(new TestModel2());

        combo.insertItemAt("TESTVALUE", 5);
        harness.check(combo.getItemAt(5), "TESTVALUE", "insertItemAt");

        combo.removeItem("TESTVALUE");
        harness.check(combo.getItemCount(), 10, "removeItem");
        
        combo.removeItemAt(0);
        harness.check(combo.getItemCount(), 9, "removeItemAt");
        
        combo.removeAllItems();
        harness.check(combo.getItemCount(), 0, "removeAllItems");

    }

}
