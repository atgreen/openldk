// Tags: JDK1.0

// Copyright (C) 1998 Cygnus Solutions

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

package gnu.testlet.java.lang.Character;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class consts implements Testlet
{
  public void test (TestHarness harness)
    {
      harness.check (Character.SPACE_SEPARATOR, 12);
      harness.check (Character.LINE_SEPARATOR, 13);
      harness.check (Character.PARAGRAPH_SEPARATOR, 14);
      harness.check (Character.UPPERCASE_LETTER, 1);
      harness.check (Character.LOWERCASE_LETTER, 2);
      harness.check (Character.TITLECASE_LETTER, 3);
      harness.check (Character.MODIFIER_LETTER, 4);
      harness.check (Character.OTHER_LETTER, 5);
      harness.check (Character.DECIMAL_DIGIT_NUMBER, 9);
      harness.check (Character.LETTER_NUMBER, 10);
      harness.check (Character.OTHER_NUMBER, 11);
      harness.check (Character.NON_SPACING_MARK, 6);
      harness.check (Character.ENCLOSING_MARK, 7);
      harness.check (Character.COMBINING_SPACING_MARK, 8);
      harness.check (Character.DASH_PUNCTUATION, 20);
      harness.check (Character.START_PUNCTUATION, 21);
      harness.check (Character.END_PUNCTUATION, 22);
      harness.check (Character.CONNECTOR_PUNCTUATION, 23);
      harness.check (Character.OTHER_PUNCTUATION, 24);
      harness.check (Character.MATH_SYMBOL, 25);
      harness.check (Character.CURRENCY_SYMBOL, 26);
      harness.check (Character.MODIFIER_SYMBOL, 27);
      harness.check (Character.OTHER_SYMBOL, 28);
      harness.check (Character.CONTROL, 15);
      harness.check (Character.FORMAT, 16);
      harness.check (Character.UNASSIGNED, 0);
      harness.check (Character.PRIVATE_USE, 18);
      harness.check (Character.SURROGATE, 19);
    }
}
