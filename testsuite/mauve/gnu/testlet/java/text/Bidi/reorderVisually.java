/* reorderVisually.java -- test bidi algorithm
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

public class reorderVisually implements Testlet
{
	public void testOne(TestHarness harness, String input, String levels,
						String expected)
	{
		Object[] inputA = new Object[input.length()];
		byte[] levelsA = new byte[levels.length()];

		for (int i = 0; i < input.length(); ++i)
		{
			inputA[i] = input.substring(i, i + 1);
			levelsA[i] = (byte) (levels.charAt(i) - '0');
		}
		
		Bidi.reorderVisually(levelsA, 0, inputA, 0, inputA.length);
		
		StringBuffer result = new StringBuffer();
		for (int i = 0; i < inputA.length; ++i)
			result.append(inputA[i]);
		
		harness.check(result.toString(), expected);
	}

	public void test(TestHarness harness)
	{
		// These tests come from unicode.org:
		// http://www.unicode.org/reports/tr9/
		testOne(harness, "car means CAR.", "00000000001110",
				"car means RAC.");
		testOne(harness, "car MEANS CAR.", "22211111111111",
				".RAC SNAEM car");
		testOne(harness, "he said \"car MEANS CAR.\"",
				"000000000222111111111100",
				"he said \"RAC SNAEM car.\"");
		testOne(harness, "DID YOU SAY 'he said \"car MEANS CAR\"'?",
				"11111111111112222222224443333333333211",
				"?'he said \"RAC SNAEM car\"' YAS UOY DID");
	}
}
