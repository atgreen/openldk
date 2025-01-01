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
 * Class to test the Korean currency.
 *
 * @author Andrew John Hughes <gnu_andrew@member.fsf.org>
 */
public class Korea implements Testlet
{

  private static final Locale TEST_LOCALE = Locale.KOREA;
  private static final String ISO4217_CODE = "KRW";
  private static final String CURRENCY_SYMBOL = "\uFFE6";
  private static final int FRACTION_DIGITS = 0;

  public void test(TestHarness harness)
  {
    Currency currency;

    /* Set default Locale for the JVM */
    Locale.setDefault(TEST_LOCALE);
    /* Get an instance of the currency */
    currency = Currency.getInstance(TEST_LOCALE);
    /* Check for the correct currency code */
    harness.check(currency.getCurrencyCode(),ISO4217_CODE, "ISO 4217 currency code retrieval check (" +
                  currency.getCurrencyCode() + ").");
    /* Check for the correct currency symbol */
    harness.check(currency.getSymbol(), CURRENCY_SYMBOL, "Currency symbol retrieval check (" +
                  currency.getSymbol() + ").");
    /* Check for the correct fraction digits */
    harness.check(currency.getDefaultFractionDigits(), FRACTION_DIGITS, "Currency fraction digits retrieval check (" +
                  currency.getDefaultFractionDigits() + ").");
    /* Check for the correct currency code from toString()*/
    harness.check(currency.toString(),ISO4217_CODE, "ISO 4217 currency code retrieval check (" + currency.toString() + ").");
  }

}
