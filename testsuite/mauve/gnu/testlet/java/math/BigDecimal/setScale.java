// Test of setScale method of BigDecimal. (PR 1596)

/*************************************************************************
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published 
/* by the Free Software Foundation, either version 2 of the License, or
/* (at your option) any later version.
/*
/* This program is distributed in the hope that it will be useful, but
/* WITHOUT ANY WARRANTY; without even the implied warranty of
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/* GNU General Public License for more details.
/*
/* You should have received a copy of the GNU General Public License
/* along with this program; if not, write to the Free Software Foundation
/* Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA
/*************************************************************************/

// Tags: JDK1.1

package gnu.testlet.java.math.BigDecimal;

import java.math.BigDecimal;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class setScale implements Testlet
{
  public void test (TestHarness harness)
  {
    harness.checkPoint ("newScale");
    BigDecimal amount = new BigDecimal ("12.34");
    BigDecimal rate = new BigDecimal ("0.06");
    BigDecimal result = amount.multiply (rate);
    try
      {
        BigDecimal foo = result.setScale(-1, BigDecimal.ROUND_UNNECESSARY);
	harness.fail ("Failed to catch ArithmeticException");
      }
    catch (ArithmeticException e)
      {
        harness.check (true);
      }
    harness.check (result.setScale(0, BigDecimal.ROUND_HALF_UP),
    		   new BigDecimal ("1"));
    harness.check (result.setScale(1, BigDecimal.ROUND_HALF_UP),
    		   new BigDecimal ("0.7"));
    harness.check (result.setScale(2, BigDecimal.ROUND_HALF_UP),
    		   new BigDecimal ("0.74"));
    harness.check (result.setScale(3, BigDecimal.ROUND_HALF_UP),
    		   new BigDecimal ("0.740"));
    harness.check (result.setScale(4, BigDecimal.ROUND_HALF_UP),
    		   new BigDecimal ("0.7404"));

    // setScale testcase from Jerry Quinn <jlquinn@optonline.net>
    harness.checkPoint ("quinn");
    BigDecimal x = new BigDecimal("0.20562");
    harness.check (x.toString(), "0.20562");
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN); // to x.xx
    harness.check (x.toString(), "0.21");

    x = new BigDecimal("0.20499");
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN);
    harness.check (x.toString(), "0.20");

    x = new BigDecimal("0.20500");
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN);
    harness.check (x.toString(), "0.20");

    x = new BigDecimal("0.20501");
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN);
    harness.check (x.toString(), "0.21");

    x = new BigDecimal("0.21499");
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN);
    harness.check (x.toString(), "0.21");

    x = new BigDecimal("0.21500");
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN);
    harness.check (x.toString(), "0.22");

    x = new BigDecimal("0.21501");
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN);
    harness.check (x.toString(), "0.22");

    // And now the negative versions.
    harness.checkPoint ("quinneg");
    x = new BigDecimal("-0.20562");
    harness.verbose(x.toString());
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN); // to x.xx
    harness.check (x.toString(), "-0.21");

    x = new BigDecimal("-0.20499");
    harness.verbose(x.toString());
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN);
    harness.check (x.toString(), "-0.20");

    x = new BigDecimal("-0.20500");
    harness.verbose(x.toString());
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN);
    harness.check (x.toString(), "-0.20");

    x = new BigDecimal("-0.20501");
    harness.verbose(x.toString());
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN);
    harness.check (x.toString(), "-0.21");

    x = new BigDecimal("-0.21499");
    harness.verbose(x.toString());
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN);
    harness.check (x.toString(), "-0.21");

    x = new BigDecimal("-0.21500");
    harness.verbose(x.toString());
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN);
    harness.check (x.toString(), "-0.22");

    x = new BigDecimal("-0.21501");
    harness.verbose(x.toString());
    x = x.setScale(2, BigDecimal.ROUND_HALF_EVEN);
    harness.check (x.toString(), "-0.22");
  }
}
