/* toString.java -- Test Field.toString
   Copyright (C) 2006 Red Hat, Inc.
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

// Tags: JDK1.1

package gnu.testlet.java.lang.reflect.Field;

import java.lang.reflect.Field;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class toString implements Testlet {

	public static final int x = 5;
	public String[] args;

	public String getFieldName(TestHarness harness, Class k, String name) {
		try {
			Field field = k.getDeclaredField(name);
			return field.toString();
		} catch (NoSuchFieldException _) {
			harness.debug(_);
			return "";
		}
	}

	public void test(TestHarness harness) {
		Class k = toString.class;

		String n1 = getFieldName(harness, k, "x");
		harness.check(n1,
				"public static final int gnu.testlet.java.lang.reflect.Field.toString.x");

		String n2 = getFieldName(harness, k, "args");
		harness.check(n2,
				"public java.lang.String[] gnu.testlet.java.lang.reflect.Field.toString.args");
	}

}
