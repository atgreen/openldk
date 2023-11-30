/*************************************************************************
/* DumpDefault11.java -- Dumps the default symbols for the US local to debug
/*
/* Copyright (c) 1999 Aaron M. Renn (arenn@urbanophile.com)
/*
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

package gnu.testlet.java.text.DecimalFormatSymbols;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

public class DumpDefault11 implements Testlet
{

public void 
test(TestHarness harness)
{
  DecimalFormatSymbols dfs = new DecimalFormatSymbols(Locale.US);

  harness.debug("decimalSeparator=" + dfs.getDecimalSeparator());
  harness.debug("digit=" + dfs.getDigit());
  harness.debug("groupingSeparator=" + dfs.getGroupingSeparator());
  harness.debug("infinity=" + dfs.getInfinity());
  harness.debug("minusSign=" + dfs.getMinusSign());
  harness.debug("NaN=" + dfs.getNaN());
  harness.debug("patternSeparator=" + dfs.getPatternSeparator());
  harness.debug("percent=" + dfs.getPercent());
  harness.debug("perMill=" + dfs.getPerMill());
  harness.debug("zeroDigit=" + dfs.getZeroDigit());
}

} // class DumpDefault11

