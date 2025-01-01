// Tags: JDK1.2

// Copyright (C) 2004 Andrew John Hughes  <gnu_andrew@member.fsf.org>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.text.SimpleDateFormat;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Locale;
import java.text.SimpleDateFormat;

/**
 * Check for correct cloning behaviour in the SimpleDateFormat
 * class.
 */
public class Localization
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness the test harness (<code>null</code> not allowed).
   */
  public void test(TestHarness harness)
  {
    SimpleDateFormat format = null;
    String standard = "GyMdkHmsSEDFwWahKzYeugAZ";
    String pattern = "EEE, d MMM yyyy HH:mm:ss Z";
    Locale locale = Locale.GERMAN;
    harness.checkPoint("German locale, standard pattern characters " +
		       "in pattern.");
    try
      {
	format = new SimpleDateFormat(pattern, locale);
	harness.check(true);
      }
    catch (IllegalArgumentException e)
      {
	harness.debug(e);
	harness.check(false);
      }
    String local = format.getDateFormatSymbols().getLocalPatternChars();
    harness.check(format.toPattern(), pattern, "Non-localized pattern " +
		  "comes back as is with toPattern().");
    String localizedPattern = translateLocalizedPattern(pattern,
							standard,
							local);
    harness.check(format.toLocalizedPattern(), localizedPattern,
		  "Non-localized pattern comes back localized with " +
		  "toLocalizedPattern().");
    harness.checkPoint("German locale, German pattern characters in pattern.");
    format = null;
    try
      {
	format = new SimpleDateFormat(localizedPattern, locale);
	harness.check(false);
      }
    catch (IllegalArgumentException e)
      {
	harness.check(true);
      }
    try
      {
	format = new SimpleDateFormat(pattern, locale);
	format.applyLocalizedPattern(localizedPattern);
	harness.check(true);
      }
    catch (IllegalArgumentException e)
      {
	harness.debug(e);
	harness.check(false);
      }
    local = format.getDateFormatSymbols().getLocalPatternChars();
    harness.check(format.toLocalizedPattern(), localizedPattern,
		  "Localized pattern comes back as is with " +
		  "toLocalizedPattern().");
    harness.check(format.toPattern(), pattern,
		  "Localized pattern comes back standardised with " +
		  "toPattern().");
  }

  /* Taken from GNU Classpath's java.text.SimpleDateFormat */
  // Copyright (C) 1998, 1999, 2000, 2001, 2003, 2004 Free Software
  // Foundation, Inc.
  /* This version has been altered to account for differing char lengths */

  /**
   * Translates either from or to a localized variant of the pattern
   * string.  For example, in the German locale, 't' (for 'tag') is
   * used instead of 'd' (for 'date').  This method translates
   * a localized pattern (such as 'ttt') to a non-localized pattern
   * (such as 'ddd'), or vice versa.  Non-localized patterns use
   * a standard set of characters, which match those of the U.S. English
   * locale.
   *
   * @param pattern the pattern to translate.
   * @param oldChars the old set of characters (used in the pattern).
   * @param newChars the new set of characters (which will be used in the
   *                 pattern).
   * @return a version of the pattern using the characters in
   *         <code>newChars</code>.
   */
  private String translateLocalizedPattern(String pattern,
					   String oldChars, String newChars)
  {
    int len = pattern.length();
    StringBuffer buf = new StringBuffer(len);
    boolean quoted = false;
    for (int i = 0;  i < len;  i++)
      {
	char ch = pattern.charAt(i);
	if (ch == '\'')
	  quoted = ! quoted;
	if (! quoted)
	  {
	    int j = oldChars.indexOf(ch);
	    if ((j >= 0) && j < newChars.length()) 
	      ch = newChars.charAt(j);
	  }
	buf.append(ch);
      }
    return buf.toString();
  }

}
