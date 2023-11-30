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
 * Class to test the reference equality of currencies.  The
 * currency class is a Singleton, so each instance for a particular
 * currency should have the same reference.
 *
 * @author Andrew John Hughes <gnu_andrew@member.fsf.org>
 */
public class ReferenceEquality implements Testlet
{

  private static final String UK_CURRENCY_CODE = "GBP";

  public void test(TestHarness harness)
  {
    Currency currency1;
    Currency currency2;
    boolean threwException;

    /* Get a UK Currency instance */
    currency1 = Currency.getInstance(Locale.UK);
    /* And another */
    currency2 = Currency.getInstance(Locale.UK);
    /* Now check their equality */
    harness.check(currency1 == currency2, "Reference equality for currencies (UK) check.");
    /* Recreate currency2 using the string code instead */
    currency2 = Currency.getInstance(UK_CURRENCY_CODE);
    /* Equality should still hold */
    harness.check(currency1 == currency2);

  }
}
