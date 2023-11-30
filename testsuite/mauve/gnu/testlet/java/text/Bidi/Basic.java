/* Basic.java -- Tests of main Bidi functionality
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

// Tags: JDK1.4

package gnu.testlet.java.text.Bidi;

import java.text.Bidi;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class Basic implements Testlet
{
	public void testOne(TestHarness harness, Bidi bidi, String expected)
	{
		StringBuffer buf = new StringBuffer();
		for (int i = 0; i < bidi.getLength(); ++i)
			buf.append(bidi.getLevelAt(i));
		harness.check(buf.toString(), expected);
	}

	public void test(TestHarness harness)
	{
		Bidi b;

		harness.checkPoint("simple");
		b = new Bidi("hi bob", Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT);
		testOne(harness, b, "000000");
		harness.check(b.baseIsLeftToRight());
		harness.check(b.isLeftToRight());
		harness.check(b.getRunCount(), 1);

		harness.checkPoint("one embedding");
		b = new Bidi("hi \u202bbob", Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT);
		testOne(harness, b, "0002222");

		harness.checkPoint("override");
		b = new Bidi("hi \u202ebob", Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT);
		testOne(harness, b, "0001111");

		harness.checkPoint("override and pop");
		b = new Bidi("car means \u202eCAR\u202c.",
				Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT);
		testOne(harness, b, "0000000000111100");
		b = new Bidi("car \u202eMEANS CAR\u202c.",
					Bidi.DIRECTION_RIGHT_TO_LEFT);
		testOne(harness, b, "2221333333333311");
	}
}
