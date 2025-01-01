/*************************************************************************
/* Test.java -- Test java.text.DateFormat
/*
/* Copyright (c) 1998 Free Software Foundation, Inc.
/* Written by Aaron M. Renn (arenn@urbanophile.com)
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

package gnu.testlet.java.text.DateFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;
import java.util.*;

public class Test extends DateFormat implements Testlet
{

public void 
test(TestHarness harness)
{
  // Do we still need to check these?  Are static finals still compiled
  // into class files?
  harness.check(ERA_FIELD, 0, "ERA_FIELD");
  harness.check(YEAR_FIELD, 1, "YEAR_FIELD");
  harness.check(MONTH_FIELD, 2, "MONTH_FIELD");
  harness.check(DATE_FIELD, 3, "DATE_FIELD");
  harness.check(HOUR_OF_DAY1_FIELD, 4, "HOUR_OF_DAY1_FIELD");
  harness.check(HOUR_OF_DAY0_FIELD, 5, "HOUR_OF_DAY0_FIELD");
  harness.check(MINUTE_FIELD, 6, "MINUTE_FIELD");
  harness.check(SECOND_FIELD, 7, "SECOND_FIELD");
  harness.check(MILLISECOND_FIELD, 8, "MILLISECOND_FIELD");
  harness.check(DAY_OF_WEEK_FIELD, 9, "DAY_OF_WEEK_FIELD");
  harness.check(DAY_OF_YEAR_FIELD, 10, "DAY_OF_YEAR_FIELD");
  harness.check(DAY_OF_WEEK_IN_MONTH_FIELD, 11, "DAY_OF_WEEK_IN_MONTH_FIELD");
  harness.check(WEEK_OF_YEAR_FIELD, 12, "WEEK_OF_YEAR_FIELD");
  harness.check(WEEK_OF_MONTH_FIELD, 13, "WEEK_OF_MONTH_FIELD");
  harness.check(AM_PM_FIELD, 14, "AM_PM_FIELD");
  harness.check(HOUR1_FIELD, 15, "HOUR1_FIELD");
  harness.check(HOUR0_FIELD, 16, "HOUR0_FIELD");
  harness.check(TIMEZONE_FIELD, 17, "TIMEZONE_FIELD");

  harness.check(FULL, 0, "FULL");
  harness.check(LONG, 1, "LONG");
  harness.check(MEDIUM, 2, "MEDIUM");
  harness.check(SHORT, 3, "SHORT");
  harness.check(DEFAULT, 2, "DEFAULT");

  Calendar c = new GregorianCalendar();
  setCalendar(c);
  harness.check(getCalendar(), c, "get/setCalendar");
  harness.check(calendar, c, "calendar");

  NumberFormat nf = NumberFormat.getNumberInstance();
  setNumberFormat(nf);
  harness.check(getNumberFormat(), nf, "get/setNumberFormat");
  harness.check(numberFormat, nf, "numberFormat");  

  setLenient(true);
  harness.check(isLenient() == true, "set/isLenient (true)");
  setLenient(false);
  harness.check(isLenient() == false, "set/isLenient (false)");

  TimeZone tz = TimeZone.getDefault();
  setTimeZone(tz);
  harness.check(getTimeZone(), tz, "get/setTimeZone");

  Object t = clone();
  harness.check(equals(t) == true, "clone/equals");

  // Hmmm.  Is this 1.2?
  //Locales[] locales = getAvailableLocales();
  //harness.debugArray(locales, "Available Locales");

  // Just to make sure we don't throw exceptions
  getInstance();
  getDateInstance(FULL, Locale.US);
  harness.check(true, "getInstance");
}

public StringBuffer
format(Date date, StringBuffer sb, FieldPosition pos)
{
  return(null);
}

public Date
parse(String text, ParsePosition pos)
{
  return(null);
}

} // class Test

