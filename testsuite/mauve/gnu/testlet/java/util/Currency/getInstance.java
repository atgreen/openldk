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

public class getInstance implements Testlet
{
  public void test(TestHarness harness)
  {
    try
      {
	// Sun uses "XXX" as the placeholder currency (when no currency is available)
        harness.check(Currency.getInstance("XXX").toString().equals("XXX"));
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }

    try
      {
        Currency.getInstance("foobar");
	harness.check(false);
      }
    catch (IllegalArgumentException _)
      {
        harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }

    try
      {
        Currency.getInstance((String)null);
	harness.check(false);
      }
    catch (NullPointerException _)
      {
        harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }

    try
      {
        Currency.getInstance(new Locale("en"));
        harness.check(false);
      }
    catch (IllegalArgumentException _)
      {
        harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }

    try
      {
        Currency.getInstance(new Locale("en", "foobar"));
	harness.check(false);
      }
    catch (IllegalArgumentException _)
      {
        harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }

    try
      {
        Currency.getInstance((Locale)null);
	harness.check(false);
      }
    catch (NullPointerException _)
      {
        harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }
  }
}
