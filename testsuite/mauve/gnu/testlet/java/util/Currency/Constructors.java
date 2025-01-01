// Tags: JDK1.4

// Copyright (C) 2004 Andrew John Hughes <gnu_andrew@member.fsf.org>

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.util.Currency;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.Currency;
import java.util.Locale;

/**
 * Class to test the currency constructors.
 *
 * @author Andrew John Hughes <gnu_andrew@member.fsf.org>
 */
public class Constructors implements Testlet
{

  private static final String INVALID_CURRENCY_CODE = "GNU";
  private static final String UK_CURRENCY_CODE = "GBP";

  public void test(TestHarness harness)
  {
    Currency currency;
    boolean threwException;

    /* Check getInstance with a null string */
    threwException = false;
    try
      {
        currency = Currency.getInstance((String) null);
      }
    catch (NullPointerException exception)
      {
        threwException = true;
      }
    harness.check(threwException, "Currency instance request with null string exception check.");
    /* Check getInstance with a non-existant ISO string */
    threwException = false;
    try
      {
        currency = Currency.getInstance(INVALID_CURRENCY_CODE);
      }
    catch (IllegalArgumentException exception)
      {
        threwException = true;
      }
    harness.check(threwException, "Currency instance request with invalid currency code string exception check.");
    /* Check getInstance with a null locale */
    threwException = false;
    try
      {
        currency = Currency.getInstance((Locale) null);
      }
    catch (NullPointerException exception)
      {
        threwException = true;
      }
    harness.check(threwException, "Currency instance request with null locale exception check.");


  }
}
