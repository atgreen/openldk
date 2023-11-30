/*************************************************************************
/* GetSet.java -- get/set method tests for java.text.DecimalFormatSymbols
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

public class GetSet11 implements Testlet
{

private char decimalSeparator = ',';
private char digit = '9';
private char groupingSeparator = '.';
private char patternSeparator = '-';
private String infinity = "infinity";
private String NaN = "NaN";
private char minusSign = '+';
private char percent = '#';
private char perMill = '!';
private char zeroDigit = 'O';

public void 
test(TestHarness harness)
{
  DecimalFormatSymbols dfs = new DecimalFormatSymbols(Locale.US);

  dfs.setDecimalSeparator(decimalSeparator);
  harness.check(dfs.getDecimalSeparator(), decimalSeparator, 
                "decimalSeparator");

  dfs.setDigit(digit);
  harness.check(dfs.getDigit(), digit, "digit");

  dfs.setGroupingSeparator(groupingSeparator);
  harness.check(dfs.getGroupingSeparator(), groupingSeparator, 
                "groupingSeparator");

  dfs.setInfinity(infinity);
  harness.check(dfs.getInfinity(), infinity, "infinity");

  dfs.setMinusSign(minusSign);
  harness.check(dfs.getMinusSign(), minusSign, "minusSign");

  dfs.setNaN(NaN);
  harness.check(dfs.getNaN(), NaN, "NaN");

  dfs.setPatternSeparator(patternSeparator);
  harness.check(dfs.getPatternSeparator(), patternSeparator,
                "patternSeparator");

  dfs.setPercent(percent);
  harness.check(dfs.getPercent(), percent, "percent");

  dfs.setPerMill(perMill);
  harness.check(dfs.getPerMill(), perMill, "perMill");

  dfs.setZeroDigit(zeroDigit);
  harness.check(dfs.getZeroDigit(), zeroDigit, "zeroDigit");
}

} // class GetSet11

