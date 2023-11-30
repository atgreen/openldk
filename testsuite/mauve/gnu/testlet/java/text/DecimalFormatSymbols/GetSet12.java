/*************************************************************************
/* GetSet12.java -- Check JDK1.2 get/set methods in DecimalFormatSymbols
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

// Tags: JDK1.2

package gnu.testlet.java.text.DecimalFormatSymbols;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

public class GetSet12 implements Testlet
{

private String currencySymbol = "@";
private String intlCurrencySymbol = "#";
private char monetarySeparator = ',';

public void 
test(TestHarness harness)
{
  DecimalFormatSymbols dfs = new DecimalFormatSymbols(Locale.US);

  dfs.setCurrencySymbol(currencySymbol);
  harness.check(dfs.getCurrencySymbol(), currencySymbol, "currencySymbol");

  dfs.setInternationalCurrencySymbol(intlCurrencySymbol);
  harness.check(dfs.getInternationalCurrencySymbol(), intlCurrencySymbol,
                "intlCurrencySymbol");

  dfs.setMonetaryDecimalSeparator(monetarySeparator);
  harness.check(dfs.getMonetaryDecimalSeparator(), monetarySeparator,
                "monetarySeparator");
}

} // class GetSet12

