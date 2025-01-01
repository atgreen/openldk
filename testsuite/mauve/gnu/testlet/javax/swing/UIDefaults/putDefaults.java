//Tags: JDK1.2

//Copyright (C) 2005 Thomas Zander <zander@kde.org>

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

package gnu.testlet.javax.swing.UIDefaults;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.UIDefaults;

/**
 * Tests if the UIDefaults accept null values and removes the key if null
 * values are beeing put into the Hashtable.
 */
public class putDefaults implements Testlet {

    public void test(TestHarness harness) {
        UIDefaults def = new UIDefaults();

	def.putDefaults(new Object[]{"foo", "bar"});
	harness.check(def.get("foo"), "bar", "simple get");
	def.putDefaults(new Object[]{"foo", null});
        try {
            def.put("foo", null);
	    Object val = def.get("foo");
            harness.check(val, (Object) null,
			  "putDefaults null equals remove");
        } catch(NullPointerException e) {
            harness.fail("putDefaults with null gave NullPointerException");
        }
    }
}
