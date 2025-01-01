/* Copyright (C) 2001, 2002 Eric Blake
 *
 * This file is part of Mauve.
 *
 * Mauve is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * Mauve is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mauve; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

// Tags: JDK1.2

package gnu.testlet.java.lang.String;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.Comparator;
import java.util.Locale;
import java.io.Serializable;

/**
 * This class tests compliance of the CASE_INSENSITIVE_ORDER field
 * of String, added in JDK 1.2.
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 */
public class CASE_INSENSITIVE_ORDER implements Testlet
{
  public void test(TestHarness harness)
  {
    Comparator c = String.CASE_INSENSITIVE_ORDER;
    harness.check(c instanceof Serializable);
    harness.check(c.compare("abc", "abc"), 0);
    harness.check(c.compare("ABC", "ABC"), 0);
    harness.check(c.compare("aBc", "AbC"), 0);
    harness.check(c.compare("", "a") < 0);
    harness.check(c.compare("a", "") > 0);
    harness.check(c.compare("a", "b") < 0);
    harness.check(c.compare("a", "B") < 0);
    harness.check(c.compare("A", "b") < 0);
    harness.check(c.compare("A", "B") < 0);
    harness.check(c.compare("b", "a") > 0);
    harness.check(c.compare("b", "A") > 0);
    harness.check(c.compare("B", "a") > 0);
    harness.check(c.compare("B", "A") > 0);

    harness.checkPoint("unicode mappings");
    // the API (as corrected in 1.4.1) specifies using
    // Character.toUpperCase(), and not String.toUpperCase(), so 1:m case
    // mappings are not performed (such as sharp-s to SS).
    harness.check(c.compare("\u00df", "sS") != 0);
    // Likewise, comparisons are locale independent, which affects things
    // like Turkish 'i' and 'I'.
    Locale l = Locale.getDefault();
    Locale.setDefault(new Locale("tr", ""));
    harness.check(c.compare("\u0131I", "i\u0130"), 0);
    Locale.setDefault(l);
    harness.check(c.compare("\u0131I", "i\u0130"), 0);

    harness.checkPoint("bad input");
    try
      {
	c.compare(null, "");
	harness.fail("expected NullPointerException");
      }
    catch (NullPointerException e)
      {
	harness.check(true);
      }
    try
      {
	c.compare("", null);
	harness.fail("expected NullPointerException");
      }
    catch (NullPointerException e)
      {
	harness.check(true);
      }
    try
      {
	c.compare(this, "");
	harness.fail("expected ClassCastException");
      }
    catch (ClassCastException e)
      {
	harness.check(true);
      }
    try
      {
	c.compare("", this);
	harness.fail("expected ClassCastException");
      }
    catch (ClassCastException e)
      {
	harness.check(true);
      }
  }
}
