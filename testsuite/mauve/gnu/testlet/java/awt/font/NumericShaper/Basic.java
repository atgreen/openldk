/* Basic.java -- Test numeric shaper functionality
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

package gnu.testlet.java.awt.font.NumericShaper;

import java.awt.font.NumericShaper;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class Basic implements Testlet
{
	public String xform(String input)
	{
		StringBuffer result = new StringBuffer();
		for (int i = 0; i < input.length(); ++i)
		{
			char c = input.charAt(i);
			if (c < ' ' || c > 0x7f)
				result.append("\\u").append(Integer.toHexString(c));
			else
				result.append(c);
		}
		return result.toString();
	}

	public void testOne(TestHarness harness, String input,
			NumericShaper shaper, String expected,
			int context)
	{
		char[] chars = input.toCharArray();
		if (context == -1)
			shaper.shape(chars, 0, chars.length);
		else
			shaper.shape(chars, 0, chars.length, context);
		// Transforming here makes the output legible in case of failure.
		harness.check(xform(new String(chars)), xform(expected));
	}

	public void test(TestHarness harness)
	{
		harness.checkPoint("non-contextual");
		NumericShaper nonc = NumericShaper.getShaper(NumericShaper.TIBETAN);
		harness.check(! nonc.isContextual());
		testOne(harness, "abc 0123", nonc,
				"abc \u0f20\u0f21\u0f22\u0f23", -1);
		testOne(harness, "abc 0123", nonc,
				"abc \u0f20\u0f21\u0f22\u0f23", NumericShaper.BENGALI);
		
		// Note that the JDK fails some of these, as ethiopic does not
		// have a digit zero.
		nonc = NumericShaper.getShaper(NumericShaper.ETHIOPIC);
		testOne(harness, "abc 0123", nonc,
				"abc 0\u1369\u136a\u136b", -1);
		testOne(harness, "abc 0123", nonc,
				"abc 0\u1369\u136a\u136b", NumericShaper.EASTERN_ARABIC);
		
		harness.checkPoint("contextual");
		NumericShaper contextualI
		  = NumericShaper.getContextualShaper(NumericShaper.KHMER
				  							  | NumericShaper.ETHIOPIC);
		NumericShaper contextualE
		  = NumericShaper.getContextualShaper(NumericShaper.KHMER
				  							  | NumericShaper.ETHIOPIC,
				  							  NumericShaper.EUROPEAN);
		harness.check(contextualE.equals(contextualI));
		harness.check(contextualE.isContextual());

		// Use built-in context.
		testOne(harness, "45", contextualE, "45", -1);
		// Explicit context.
		testOne(harness, "45", contextualE, "\u17e4\u17e5", NumericShaper.KHMER);
		// Explicit but unrecognized context.
		testOne(harness, "45", contextualE, "45", NumericShaper.ARABIC);
		// The other explicit context.
		testOne(harness, "45", contextualE, "\u136c\u136d", NumericShaper.ETHIOPIC);
		// Context from the text.  The first \\u is ethiopic, the second khmer.
		testOne(harness, "45 \u134d 045 \u1782 045", contextualI,
				"45 \u134d 0\u136c\u136d \u1782 \u17e0\u17e4\u17e5", -1);

		harness.checkPoint("arabic");
		NumericShaper arabic
		  = NumericShaper.getContextualShaper(NumericShaper.ARABIC
				  | NumericShaper.EASTERN_ARABIC);
		// According to testing, eastern arabic takes precedence here.
		testOne(harness, "\u062b 1", arabic, "\u062b \u06f1", -1);
		// This should choose eastern arabic.
		testOne(harness, "1", arabic, "\u06f1", NumericShaper.EASTERN_ARABIC);
	}
}
