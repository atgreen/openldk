// Tags: JDK1.2
// Uses: TestModel1

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

/** Tests selection and counting methods of JComboBox with a ComboBoxModel
 * that extends AbstractListModel and implements ComboBoxModel.
 *
 * <p>The order of operations is important. That means if one of the
 * tests fails its likely that following tests fail too.</p>
 * <p>The test were written after fixing GNU Classpath bug #11255.</p> 
 *  
 * @author Robert Schuster
 */
public class SimpleSelectionTest implements Testlet
{

    public void test(TestHarness harness)
    {
        JComboBox combo = new JComboBox(new TestModel1());

        /* Tests returning item count with a data model which is not an instance
         * of DefaultComboBoxModel.
         */
        harness.check(combo.getItemCount(), 4, "get item count");

        /* Tests whether the JComboBox does not change the value that
         * the model decided to preselect.
         */
        harness.check(combo.getSelectedItem(), "In", "preselected item");

        /* Tests selecting by value. */
        combo.setSelectedItem("Freedom");
        harness.check(combo.getSelectedIndex(), 3, "select by value");

        /* Tests whether selecting by value with an item that is not
         * in the model has no effect.
         */
        combo.setSelectedItem("BOGUS");
        harness.check(combo.getSelectedIndex(), 3, "select by unknown value");

        /* Tests returning the 2nd item via index. */
        harness.check(combo.getItemAt(1), "As", "return via index");

        /* Tests unselecting via index.
         * Note that for unselecting to work properly the ComboBoxModel must support that.
         */
        combo.setSelectedIndex(-1);
        harness.check(combo.getSelectedItem(), null, "unselect via index");

        /* Tests IllegalArgumentException being thrown for an index
         * below -1.
         */
        try
        {
            combo.setSelectedIndex(-12);
            harness.fail("illegal index access 1: exception not thrown");
        }
        catch (Exception e)
        {
            harness.check(
                e instanceof IllegalArgumentException,
                "illegal index access 1");
        }

        /* Tests IllegalArgumentException being thrown for an index
         * outside the upper bound.
         */
        try
        {
            combo.setSelectedIndex(1200);
            harness.fail("illegal index access 2: exception not thrown");
        }
        catch (Exception e)
        {
            harness.check(
                e instanceof IllegalArgumentException,
                "illegal index access 2");
        }

        /* Tests selection via index. */
        combo.setSelectedIndex(0);
        harness.check(
            combo.getSelectedItem(),
            "Free",
            "valid selection via index");

        /* Tests unselecting via null.
         * Note that for unselecting to work properly the ComboBoxModel must support that.
         */
        combo.setSelectedItem(null);
        harness.check(combo.getSelectedItem(), null, "unselect via null");

    }

}
