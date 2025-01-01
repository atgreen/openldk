// Tags: JDK1.0

// Copyright (C) 2004, 2005 Michael Koch <konqueror@gmx.de>

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

package gnu.testlet.locales;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.text.*;
import java.util.*;

public class LocaleTest
  implements Testlet
{
  public class ExpectedValues
  {
    String language;
    String country;
    String variant;
    String localeStr;
    String iso3language;
    String iso3country;
    String displayLanguage;
    String displayCountry;
    String displayVariant;
    String displayName;
    String currencyCode;
    int currencyFractionDigits;
    String currencySymbol;
    
    public ExpectedValues(String language, String country, String variant, String localeStr,
			  String iso3language, String iso3country,
			  String displayLanguage, String displayCountry,
			  String displayVariant, String displayName,
			  String currencyCode, int currencyFractionDigits,
			  String currencySymbol)
    {
      this.language = language;
      this.country = country;
      this.variant = variant;
      this.localeStr = localeStr;
      this.iso3language = iso3language;
      this.iso3country = iso3country;
      this.displayLanguage = displayLanguage;
      this.displayCountry = displayCountry;
      this.displayVariant = displayVariant;
      this.displayName = displayName;
      this.currencyCode = currencyCode;
      this.currencyFractionDigits = currencyFractionDigits;
      this.currencySymbol = currencySymbol;
    }
  }

  public class ExpectedDateValues
  {
    String a, b, c, d, e, f, g, h;
    
    public ExpectedDateValues(String a, String b, String c, String d, String e, String f, String g, String h)
    {
      this.a = a;
      this.b = b;
      this.c = c;
      this.d = d;
      this.e = e;
      this.f = f;
      this.g = g;
      this.h = h;
    }
  }

  public class ExpectedNumberValues
  {
    String a, b, c, d, e;
    
    public ExpectedNumberValues(String a, String b, String c, String d, String e)
    {
      this.a = a;
      this.b = b;
      this.c = c;
      this.d = d;
      this.e = e;
    }
  }

  private void checkLocale(TestHarness h, Locale locale,
			   ExpectedValues expected,
			   ExpectedDateValues expectedDate,
			   ExpectedNumberValues expectedNumber1,
			   ExpectedNumberValues expectedNumberCurrency1,
			   ExpectedNumberValues expectedNumberCurrency2,
			   ExpectedNumberValues expectedNumber3,
			   ExpectedNumberValues expectedNumber4,
			   ExpectedNumberValues expectedNumberProcent)
  {
    h.checkPoint("Locale " + locale);

    // Force GERMAN as default locale.
    Locale.setDefault(Locale.GERMAN);
    TimeZone.setDefault(TimeZone.getTimeZone("GMT"));   
 
    // Locale
    
    if (expected != null)
      {
	h.check(locale.getLanguage(), expected.language);
	h.check(locale.getCountry(), expected.country);
	h.check(locale.getVariant(), expected.variant);
	h.check(locale.toString(), expected.localeStr);
	h.check(locale.getISO3Language(), expected.iso3language);
	h.check(locale.getISO3Country(), expected.iso3country);
	h.check(locale.getDisplayLanguage(), expected.displayLanguage);
	h.check(locale.getDisplayCountry(), expected.displayCountry);
	h.check(locale.getDisplayVariant(), expected.displayVariant);
	h.check(locale.getDisplayName(), expected.displayName);
      }

    // Date and time formats
    h.debug("Locale " + locale + " date/time formats");

    if (expectedDate != null)
      {
	DateFormat df;

	Date date1 = new Date(74, 2, 18, 17, 20, 30);
    
	// Date instance.
	
	df = DateFormat.getDateInstance(DateFormat.DEFAULT, locale);
	h.check(df.format(date1), expectedDate.a, "DateFormat.DEFAULT "+ locale);

	df = DateFormat.getDateInstance(DateFormat.SHORT, locale);
	h.check(df.format(date1), expectedDate.b, "DateFormat.SHORT "+ locale);

	df = DateFormat.getDateInstance(DateFormat.MEDIUM, locale);
	h.check(df.format(date1), expectedDate.c, "DateFormat.MEDIUM "+ locale);

	df = DateFormat.getDateInstance(DateFormat.LONG, locale);
	h.check(df.format(date1), expectedDate.d, "DateFormat.LONG "+ locale);

	// Assume DEFAULT == MEDIUM
	df = DateFormat.getDateInstance(DateFormat.DEFAULT, locale);
	h.check(df.format(date1), expectedDate.c, "DateFormat.DEFAULT == DateFormat.MEDIUM "+ locale);
	
	// Time instance.
	
	df = DateFormat.getTimeInstance(DateFormat.DEFAULT, locale);
	h.check(df.format(date1), expectedDate.e, "DateFormat.DEFAULT "+ locale);

	df = DateFormat.getTimeInstance(DateFormat.SHORT, locale);
	h.check(df.format(date1), expectedDate.f, "DateFormat.SHORT "+ locale);

	df = DateFormat.getTimeInstance(DateFormat.MEDIUM, locale);
	h.check(df.format(date1), expectedDate.g, "DateFormat.MEDIUM "+ locale);

	df = DateFormat.getTimeInstance(DateFormat.LONG, locale);
	h.check(df.format(date1), expectedDate.h, "DateFormat.LONG "+ locale);

	// Assume DEFAULT == MEDIUM
	df = DateFormat.getTimeInstance(DateFormat.DEFAULT, locale);
	h.check(df.format(date1), expectedDate.g, "DateFormat.DEFAULT == DateFormat.MEDIUM "+ locale);
      }

    h.checkPoint("numberformats locale: "+ locale);
    // Number formats
    
    NumberFormat nf;
    
    if (expectedNumber1 != null)
      {
	nf = NumberFormat.getInstance(locale);

	h.check(nf.format(1000L), expectedNumber1.a);
	h.check(nf.format(1000000L), expectedNumber1.b);
	h.check(nf.format(100d), expectedNumber1.c);
	h.check(nf.format(100.1234d), expectedNumber1.d);
	h.check(nf.format(10000000.1234d), expectedNumber1.e);
      }
    
    if (expectedNumberCurrency1 != null)
      {
	nf = NumberFormat.getCurrencyInstance(locale);
    
	h.check(nf.format(1000L), expectedNumberCurrency1.a);
	h.check(nf.format(1000000L), expectedNumberCurrency1.b);
	h.check(nf.format(100d), expectedNumberCurrency1.c);
	h.check(nf.format(100.1234d), expectedNumberCurrency1.d);
	h.check(nf.format(10000000.1234d), expectedNumberCurrency1.e);
      }
    
    if (expectedNumberCurrency2 != null)
      {
	nf = NumberFormat.getCurrencyInstance(locale);
    
	h.check(nf.format(-1000L), expectedNumberCurrency2.a);
	h.check(nf.format(-1000000L), expectedNumberCurrency2.b);
	h.check(nf.format(-100d), expectedNumberCurrency2.c);
	h.check(nf.format(-100.1234d), expectedNumberCurrency2.d);
	h.check(nf.format(-10000000.1234d), expectedNumberCurrency2.e);
      }
    
    if (expectedNumber3 != null)
      {
	nf = NumberFormat.getIntegerInstance(locale);
    
	h.check(nf.format(1000L), expectedNumber3.a);
	h.check(nf.format(1000000L), expectedNumber3.b);
	h.check(nf.format(100d), expectedNumber3.c);
	h.check(nf.format(100.1234d), expectedNumber3.d);
	h.check(nf.format(10000000.1234d), expectedNumber3.e);
      }
    
    if (expectedNumber4 != null)
      {
	nf = NumberFormat.getNumberInstance(locale);
    
	h.check(nf.format(1000L), expectedNumber4.a);
	h.check(nf.format(1000000L), expectedNumber4.b);
	h.check(nf.format(100d), expectedNumber4.c);
	h.check(nf.format(100.1234d), expectedNumber4.d);
	h.check(nf.format(10000000.1234d), expectedNumber4.e);
      }
    
    if (expectedNumberProcent != null)
      {
	nf = NumberFormat.getPercentInstance(locale);
    
	h.check(nf.format(1000L), expectedNumberProcent.a);
	h.check(nf.format(1000000L), expectedNumberProcent.b);
	h.check(nf.format(100d), expectedNumberProcent.c);
	h.check(nf.format(100.1234d), expectedNumberProcent.d);
	h.check(nf.format(10000000.1234d), expectedNumberProcent.e);
      }
    
    // Currencies
    h.checkPoint("Currencies locale: "+ locale);

    if (expected != null)
      {
	Currency currency = Currency.getInstance(locale);

	h.check(currency.getCurrencyCode(), expected.currencyCode);
	h.check(currency.getDefaultFractionDigits(), expected.currencyFractionDigits);
	h.check(currency.getSymbol(), expected.currencySymbol);

	try
	  {
	    Currency byCode = Currency.getInstance(currency.getCurrencyCode());

	    h.check(currency.getCurrencyCode(), byCode.getCurrencyCode());
	    h.check(currency.getDefaultFractionDigits(), byCode.getDefaultFractionDigits());
	    h.check(currency.getSymbol(), byCode.getSymbol());
	  }
	catch (IllegalArgumentException e)
	  {
	    h.fail("Currency code not supported: " + currency.getCurrencyCode());
	  }
      }
  }
  
  public void test(TestHarness h)
  {
    // Check all supported locales.
    
    // FIXME: Add all EURO countries.
    
    // Locale: Germany
    checkLocale(h, new Locale("de", "DE"),
		new ExpectedValues("de", "DE", "", "de_DE", "deu", "DEU",
			           "Deutsch", "Deutschland", "", "Deutsch (Deutschland)",
				   "EUR", 2, "EUR"),
		new ExpectedDateValues("18.03.1974", "18.03.74", "18.03.1974", "18. M\u00e4rz 1974", "17:20:30", "17:20", "17:20:30", "17:20:30 GMT"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100,123", "10.000.000,123"),
		new ExpectedNumberValues("1.000,00 \u20ac", "1.000.000,00 \u20ac", "100,00 \u20ac", "100,12 \u20ac", "10.000.000,12 \u20ac"),
		new ExpectedNumberValues("-1.000,00 \u20ac", "-1.000.000,00 \u20ac", "-100,00 \u20ac", "-100,12 \u20ac", "-10.000.000,12 \u20ac"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100", "10.000.000"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100,123", "10.000.000,123"),
		new ExpectedNumberValues("100.000%", "100.000.000%", "10.000%", "10.012%", "1.000.000.012%"));
    // Locale: Belgium
    checkLocale(h, new Locale("fr", "BE"),
		new ExpectedValues("fr", "BE", "", "fr_BE", "fra", "BEL",
				   "Franz\u00f6sisch", "Belgien", "", "Franz\u00f6sisch (Belgien)",
				   "EUR", 2, "EUR"),
		new ExpectedDateValues("18-mars-1974", "18/03/74", "18-mars-1974", "18 mars 1974", "17:20:30", "17:20", "17:20:30", "17:20:30 GMT"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100,123", "10.000.000,123"),
		new ExpectedNumberValues("1.000,00 \u20ac", "1.000.000,00 \u20ac", "100,00 \u20ac", "100,12 \u20ac", "10.000.000,12 \u20ac"),
		new ExpectedNumberValues("-1.000,00 \u20ac", "-1.000.000,00 \u20ac", "-100,00 \u20ac", "-100,12 \u20ac", "-10.000.000,12 \u20ac"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100", "10.000.000"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100,123", "10.000.000,123"),
		new ExpectedNumberValues("100.000 %", "100.000.000 %", "10.000 %", "10.012 %", "1.000.000.012 %"));
    // Locale: Greece
    // FIXME: Disabled for now due to pattern problems.
    /*
    checkLocale(h, new Locale("el", "GR"),
		new ExpectedValues("el", "GR", "", "el_GR", "ell", "GRC",
				   "Griechisch", "Griechenland", "", "Griechisch (Griechenland)",
				   "EUR", 2, "\u20ac"),
		new ExpectedDateValues("18.03.1974", "18.03.74", "18.03.1974", "18. M\u00e4rz 1974", "17:20:30", "17:20", "17:20:30", "17:20:30 GMT"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100,123", "10.000.000,123"),
		new ExpectedNumberValues("1.000,00 \u20ac", "1.000.000,00 \u20ac", "100,00 \u20ac", "100,12 \u20ac", "10.000.000,12 \u20ac"),
		new ExpectedNumberValues("-1.000,00 \u20ac", "-1.000.000,00 \u20ac", "-100,00 \u20ac", "-100,12 \u20ac", "-10.000.000,12 \u20ac"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100", "10.000.000"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100,123", "10.000.000,123"),
		new ExpectedNumberValues("100.000%", "100.000.000%", "10.000%", "10.012%", "1.000.000.012%"));
    */
    // Locale: Ireland
    checkLocale(h, new Locale("en", "IE"),
		new ExpectedValues("en", "IE", "", "en_IE", "eng", "IRL",
				   "Englisch", "Irland", "", "Englisch (Irland)",
				   "EUR", 2, "EUR"),
		new ExpectedDateValues("18-Mar-1974", "18/03/74", "18-Mar-1974", "18 March 1974", "17:20:30", "17:20", "17:20:30", "17:20:30 GMT"),
		new ExpectedNumberValues("1,000", "1,000,000", "100", "100.123", "10,000,000.123"),
		new ExpectedNumberValues("\u20ac1,000.00", "\u20ac1,000,000.00", "\u20ac100.00", "\u20ac100.12", "\u20ac10,000,000.12"),
		new ExpectedNumberValues("-\u20ac1,000.00", "-\u20ac1,000,000.00", "-\u20ac100.00", "-\u20ac100.12", "-\u20ac10,000,000.12"),
		new ExpectedNumberValues("1,000", "1,000,000", "100", "100", "10,000,000"),
		new ExpectedNumberValues("1,000", "1,000,000", "100", "100.123", "10,000,000.123"),
		new ExpectedNumberValues("100,000%", "100,000,000%", "10,000%", "10,012%", "1,000,000,012%"));
    // Locale: France
    checkLocale(h, new Locale("fr", "FR"),
		new ExpectedValues("fr", "FR", "", "fr_FR", "fra", "FRA",
				   "Franz\u00f6sisch", "Frankreich", "", "Franz\u00f6sisch (Frankreich)",
				   "EUR", 2, "EUR"),
		null,
		null,
		null,
		null,
		null,
		null,
		null);
    // Locale: Spain
    checkLocale(h, new Locale("es", "ES"),
		new ExpectedValues("es", "ES", "", "es_ES", "spa", "ESP",
				   "Spanisch", "Spanien", "", "Spanisch (Spanien)",
				   "EUR", 2, "EUR"),
		null,
		null,
		null,
		null,
		null,
		null,
		null);
    // Locale: Portugal
    checkLocale(h, new Locale("pt", "PT"),
		new ExpectedValues("pt", "PT", "", "pt_PT", "por", "PRT",
				   "Portugiesisch", "Portugal", "", "Portugiesisch (Portugal)",
				   "EUR", 2, "EUR"),
		null,
		null,
		null,
		null,
		null,
		null,
		null);
    // Locale: Italy
    checkLocale(h, new Locale("it", "IT"),
		new ExpectedValues("it", "IT", "", "it_IT", "ita", "ITA",
				   "Italienisch", "Italien", "", "Italienisch (Italien)",
				   "EUR", 2, "EUR"),
		null,
		null,
		null,
		null,
		null,
		null,
		null);
    // Locale: The Netherlands
    checkLocale(h, new Locale("nl", "NL"),
		new ExpectedValues("nl", "NL", "", "nl_NL", "nld", "NLD",
				   "Niederl\u00e4ndisch", "Niederlande", "", "Niederl\u00e4ndisch (Niederlande)",
				   "EUR", 2, "EUR"),
		new ExpectedDateValues("18-mrt-1974", "18-3-74", "18-mrt-1974", "18 maart 1974", "17:20:30", "17:20", "17:20:30", "17:20:30 GMT"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100,123", "10.000.000,123"),
		new ExpectedNumberValues("\u20ac 1.000,00", "\u20ac 1.000.000,00", "\u20ac 100,00", "\u20ac 100,12", "\u20ac 10.000.000,12"),
		new ExpectedNumberValues("\u20ac 1.000,00-", "\u20ac 1.000.000,00-", "\u20ac 100,00-", "\u20ac 100,12-", "\u20ac 10.000.000,12-"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100", "10.000.000"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100,123", "10.000.000,123"),
		new ExpectedNumberValues("100.000%", "100.000.000%", "10.000%", "10.012%", "1.000.000.012%"));
    // Locale: Luxemborg
    checkLocale(h, new Locale("fr", "LU"),
		new ExpectedValues("fr", "LU", "", "fr_LU", "fra", "LUX",
				   "Franz\u00f6sisch", "Luxemburg", "", "Franz\u00f6sisch (Luxemburg)",
				   "EUR", 2, "EUR"),
		null,
		null,
		null,
		null,
		null,
		null,
		null);
    // Locale: United Kingdom
    checkLocale(h, Locale.UK,
		new ExpectedValues("en", "GB", "", "en_GB", "eng", "GBR",
				   "Englisch", "Vereinigtes K\u00f6nigreich", "", "Englisch (Vereinigtes K\u00f6nigreich)",
				   "GBP", 2, "GBP"),
		new ExpectedDateValues("18-Mar-1974", "18/03/74", "18-Mar-1974", "18 March 1974", "17:20:30", "17:20", "17:20:30", "17:20:30 GMT"),
		new ExpectedNumberValues("1,000", "1,000,000", "100", "100.123", "10,000,000.123"),
		new ExpectedNumberValues("\u00a31,000.00", "\u00a31,000,000.00", "\u00a3100.00", "\u00a3100.12", "\u00a310,000,000.12"),
		new ExpectedNumberValues("-\u00a31,000.00", "-\u00a31,000,000.00", "-\u00a3100.00", "-\u00a3100.12", "-\u00a310,000,000.12"),
		new ExpectedNumberValues("1,000", "1,000,000", "100", "100", "10,000,000"),
		new ExpectedNumberValues("1,000", "1,000,000", "100", "100.123", "10,000,000.123"),
		new ExpectedNumberValues("100,000%", "100,000,000%", "10,000%", "10,012%", "1,000,000,012%"));
    // Locale: United States
    checkLocale(h, Locale.US,
		new ExpectedValues("en", "US", "", "en_US", "eng", "USA",
				   "Englisch", "Vereinigte Staaten von Amerika", "", "Englisch (Vereinigte Staaten von Amerika)",
				   "USD", 2, "USD"),
		new ExpectedDateValues("Mar 18, 1974", "3/18/74", "Mar 18, 1974", "March 18, 1974", "5:20:30 PM", "5:20 PM", "5:20:30 PM", "5:20:30 PM GMT"),
		new ExpectedNumberValues("1,000", "1,000,000", "100", "100.123", "10,000,000.123"),
		new ExpectedNumberValues("$1,000.00", "$1,000,000.00", "$100.00", "$100.12", "$10,000,000.12"),
		new ExpectedNumberValues("($1,000.00)", "($1,000,000.00)", "($100.00)", "($100.12)", "($10,000,000.12)"),
		new ExpectedNumberValues("1,000", "1,000,000", "100", "100", "10,000,000"),
		new ExpectedNumberValues("1,000", "1,000,000", "100", "100.123", "10,000,000.123"),
		new ExpectedNumberValues("100,000%", "100,000,000%", "10,000%", "10,012%", "1,000,000,012%"));
    // Locale: Finland
    checkLocale(h, new Locale("fi", "FI"),
		new ExpectedValues("fi", "FI", "", "fi_FI", "fin", "FIN",
				   "Finnisch", "Finnland", "", "Finnisch (Finnland)",
				   "EUR", 2, "EUR"),
		new ExpectedDateValues("18.3.1974", "18.3.1974", "18.3.1974", "18. maaliskuuta 1974", "17:20:30", "17:20", "17:20:30", "klo 17.20.30"),
		new ExpectedNumberValues("1\u00a0000", "1\u00a0000\u00a0000", "100", "100,123", "10\u00a0000\u00a0000,123"),
		new ExpectedNumberValues("1\u00a0000,00 \u20ac", "1\u00a0000\u00a0000,00 \u20ac", "100,00 \u20ac", "100,12 \u20ac", "10\u00a0000\u00a0000,12 \u20ac"),
		new ExpectedNumberValues("-1\u00a0000,00 \u20ac", "-1\u00a0000\u00a0000,00 \u20ac", "-100,00 \u20ac", "-100,12 \u20ac", "-10\u00a0000\u00a0000,12 \u20ac"),
		new ExpectedNumberValues("1\u00a0000", "1\u00a0000\u00a0000", "100", "100", "10\u00a0000\u00a0000"),
		new ExpectedNumberValues("1\u00a0000", "1\u00a0000\u00a0000", "100", "100,123", "10\u00a0000\u00a0000,123"),
		new ExpectedNumberValues("100\u00a0000%", "100\u00a0000\u00a0000%", "10\u00a0000%", "10\u00a0012%", "1\u00a0000\u00a0000\u00a0012%"));
    // Locale: Turkey
    checkLocale(h, new Locale("tr", "TR"),
		new ExpectedValues("tr", "TR", "", "tr_TR", "tur", "TUR",
				   "T\u00fcrkisch", "T\u00fcrkei", "", "T\u00fcrkisch (T\u00fcrkei)",
				   "TRY", 2, "TRY"),
		new ExpectedDateValues("18.Mar.1974", "18.03.1974", "18.Mar.1974", "18 Mart 1974 Pazartesi", "17:20:30", "17:20", "17:20:30", "17:20:30 GMT"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100,123", "10.000.000,123"),
		new ExpectedNumberValues("1.000,00 YTL", "1.000.000,00 YTL", "100,00 YTL", "100,12 YTL", "10.000.000,12 YTL"),
		new ExpectedNumberValues("-1.000,00 YTL", "-1.000.000,00 YTL", "-100,00 YTL", "-100,12 YTL", "-10.000.000,12 YTL"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100", "10.000.000"),
		new ExpectedNumberValues("1.000", "1.000.000", "100", "100,123", "10.000.000,123"),
		new ExpectedNumberValues("% 100.000", "% 100.000.000", "% 10.000", "% 10.012", "% 1.000.000.012"));
    // Locale: Kazakstan
    checkLocale(h, new Locale("kk", "KZ"),
		new ExpectedValues("kk", "KZ", "", "kk_KZ", "kaz", "KAZ",
				   "Kasachisch", "Kasachstan", "", "Kasachisch (Kasachstan)",
				   "KZT", 2, "KZT"),
		null,
		null,
		null,
		null,
		null,
		null,
		null);
    // Locale: Estonia
    checkLocale(h, new Locale("et", "EE"),
		new ExpectedValues("et", "EE", "", "et_EE", "est", "EST",
				   "Estnisch", "Estland", "", "Estnisch (Estland)",
				   "EEK", 2, "EEK"),
		new ExpectedDateValues("18.03.1974", "18.03.74", "18.03.1974", "esmasp\u00e4ev, 18. M\u00e4rts 1974. a", "17:20:30", "17:20", "17:20:30", "17:20:30 GMT"),
		new ExpectedNumberValues("1\u00a0000", "1\u00a0000\u00a0000", "100", "100,123", "10\u00a0000\u00a0000,123"),
		new ExpectedNumberValues("1\u00a0000 kr", "1\u00a0000\u00a0000 kr", "100 kr", "100,12 kr", "10\u00a0000\u00a0000,12 kr"),
		new ExpectedNumberValues("-1\u00a0000 kr", "-1\u00a0000\u00a0000 kr", "-100 kr", "-100,12 kr", "-10\u00a0000\u00a0000,12 kr"),
		new ExpectedNumberValues("1\u00a0000", "1\u00a0000\u00a0000", "100", "100", "10\u00a0000\u00a0000"),
		new ExpectedNumberValues("1\u00a0000", "1\u00a0000\u00a0000", "100", "100,123", "10\u00a0000\u00a0000,123"),
		new ExpectedNumberValues("100\u00a0000%", "100\u00a0000\u00a0000%", "10\u00a0000%", "10\u00a0012%", "1\u00a0000\u00a0000\u00a0012%"));
  }
}

