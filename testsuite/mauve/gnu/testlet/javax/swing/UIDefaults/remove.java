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

public class remove implements Testlet {

    public void test(TestHarness harness) {
        UIDefaults def = new UIDefaults();
        def.put("foo", "bar");
        def.put("foo2", "bar2");
        harness.check(def.get("foo"), "bar", "simple get");
        harness.check(def.get("foo2"), "bar2");
        try {
            def.put("foo", null);
            harness.check(def.get("foo"), (Object) null, "put null equals remove");
        } catch(NullPointerException e) {
            harness.fail("put with null gave NullPointerException");
        }
        harness.checkPoint("rest intact?");
        harness.check(def.get("foo2"), "bar2");
    }
}
