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

/** This tests the methods of JComboBox that would modify the
 * <code>ComboBoxModel</code>. Since no
 * <code>MutableComboBoxModel</code> is given
 * <code>RuntimeException</code>S are expected to be
 * thrown instead.
 * 
 * @author Robert Schuster
 */
public class MutableTest1 implements Testlet
{

    public void test(TestHarness harness)
    {
    	JComboBox combo = new JComboBox(new TestModel1());

        boolean ok = false;

        try
        {
            combo.insertItemAt("BOGUS", 0);
        }
        catch (RuntimeException re)
        {
            ok = true;
        }
        harness.check(ok, "insertItemAt");

        ok = false;
        try
        {
            combo.removeItem("BOGUS");
        }
        catch (RuntimeException re)
        {
            ok = true;
        }
        harness.check(ok, "removeItem");

        ok = false;
        try
        {
            combo.removeItemAt(0);
        }
        catch (RuntimeException re)
        {
            ok = true;
        }
        harness.check(ok, "removeItemAt");
        ok = false;

        try
        {
            combo.removeAllItems();
        }
        catch (RuntimeException re)
        {
            ok = true;
        }
        harness.check(ok, "removeAllItems");
    }

}
