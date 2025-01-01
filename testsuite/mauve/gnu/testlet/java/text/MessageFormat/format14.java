// Tags: JDK1.4

// Copyright (C) 2004, 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.text.MessageFormat;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.ChoiceFormat;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.MessageFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

/**
 * Some checks for the format() methods in the {@link MessageFormat} class.
 */
public class format14 implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    testStaticFormat(harness);
    //testConstructor2(harness);
  }
  
  /**
   * Some checks for the static format(String, Object[]) method.
   * 
   * @param harness  the test harness.
   */
  private void testStaticFormat(TestHarness harness) 
  {
    harness.checkPoint("static (String, Object[])");
    
    // basic string check
    String s = MessageFormat.format("{0}", new Object[] {"ABC"});
    harness.check(s, "ABC");
    
    s = MessageFormat.format("-{0}-", new Object[] {"ABC"});
    harness.check(s, "-ABC-");
    
    // basic number checks
    harness.checkPoint("number");
    s = MessageFormat.format("{0,number}", new Object[] {new Integer(9999)});
    String expected = NumberFormat.getInstance(Locale.getDefault()).format(9999);
    harness.check(s, expected);
    
    s = MessageFormat.format("{0,number,integer}", new Object[] {new Integer(9999)});
    expected = NumberFormat.getIntegerInstance(Locale.getDefault()).format(9999);
    harness.check(s, expected);

    s = MessageFormat.format("{0,number,currency}", new Object[] {new Integer(9999)});
    expected = NumberFormat.getCurrencyInstance(Locale.getDefault()).format(9999);
    harness.check(s, expected);

    s = MessageFormat.format("{0,number,percent}", new Object[] {new Integer(9999)});
    expected = NumberFormat.getPercentInstance(Locale.getDefault()).format(9999);
    harness.check(s, expected);

    s = MessageFormat.format("{0,number,#,##0.00}", new Object[] {new Integer(9999)});
    expected = new DecimalFormat("#,##0.00", new DecimalFormatSymbols(Locale.getDefault())).format(9999);
    harness.check(s, expected);

    // basic date checks
    harness.checkPoint("date");
    Date t = new Date();
    s = MessageFormat.format("{0,date}", new Object[] {t});
    expected = DateFormat.getDateInstance(DateFormat.DEFAULT, Locale.getDefault()).format(t);
    harness.check(s, expected);
    
    s = MessageFormat.format("{0,date,short}", new Object[] {t});
    expected = DateFormat.getDateInstance(DateFormat.SHORT, Locale.getDefault()).format(t);
    harness.check(s, expected);

    try  // don't let failure disturb remaining tests 
    {
      s = MessageFormat.format("{0,date,medium}", new Object[] {t});
      expected = DateFormat.getDateInstance(DateFormat.DEFAULT, Locale.getDefault()).format(t);
      harness.check(s, expected);
    }
    catch (Exception e) 
    {
      harness.debug(e);
      harness.check(false);
    }
    
    s = MessageFormat.format("{0,date,long}", new Object[] {t});
    expected = DateFormat.getDateInstance(DateFormat.LONG, Locale.getDefault()).format(t);
    harness.check(s, expected);

    s = MessageFormat.format("{0,date,full}", new Object[] {t});
    expected = DateFormat.getDateInstance(DateFormat.FULL, Locale.getDefault()).format(t);
    harness.check(s, expected);

    s = MessageFormat.format("{0,date,dd-MMM-yyyy}", new Object[] {t});
    expected = new SimpleDateFormat("dd-MMM-yyyy", Locale.getDefault()).format(t);
    harness.check(s, expected);

    // basic time checks
    harness.checkPoint("time");
    s = MessageFormat.format("{0,time}", new Object[] {t});
    expected = DateFormat.getTimeInstance(DateFormat.DEFAULT, Locale.getDefault()).format(t);
    harness.check(s, expected);
    
    s = MessageFormat.format("{0,time,short}", new Object[] {t});
    expected = DateFormat.getTimeInstance(DateFormat.SHORT, Locale.getDefault()).format(t);
    harness.check(s, expected);

    try  // don't let failure disturb remaining tests 
    {
      s = MessageFormat.format("{0,time,medium}", new Object[] {t});
      expected = DateFormat.getTimeInstance(DateFormat.DEFAULT, Locale.getDefault()).format(t);
      harness.check(s, expected);
    }
    catch (Exception e) 
    {
      harness.debug(e);
      harness.check(false);
    }
    
    s = MessageFormat.format("{0,time,long}", new Object[] {t});
    expected = DateFormat.getTimeInstance(DateFormat.LONG, Locale.getDefault()).format(t);
    harness.check(s, expected);

    s = MessageFormat.format("{0,time,full}", new Object[] {t});
    expected = DateFormat.getTimeInstance(DateFormat.FULL, Locale.getDefault()).format(t);
    harness.check(s, expected);

    s = MessageFormat.format("{0,time,hh:mm}", new Object[] {t});
    expected = new SimpleDateFormat("hh:mm", Locale.getDefault()).format(t);
    harness.check(s, expected);

    harness.checkPoint("choice");
    
    try
    {
      s = MessageFormat.format("{0,choice,0#zero|1#one|1<many}", new Object[]{new Double(2.0)});
      expected = new ChoiceFormat("0#zero|1#one|1<many").format(new Double(2.0));
      harness.check(s, expected);
    }
    catch (Exception e)
    {
      harness.debug(e);
      harness.check(false);
    }
    
    harness.checkPoint("miscellaneous");
    
    // check for null string
    try 
    {
      /* String s = */ MessageFormat.format(null, new Object[] {"X"});
      harness.check(false);
    }
    catch (NullPointerException e) 
    {
      harness.check(true);
    }
    
    // check for null array
    s = MessageFormat.format("{0}", null);
    harness.check(s, "{0}");
    
    // check for null item in array
    s = MessageFormat.format("{0}", new Object[] {null});
    harness.check(s, "null");
    
    // check for bad types
    try 
    {
      s = MessageFormat.format("{0,number}", new Object[] {"X"});
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }

    harness.checkPoint("quoting");

    // Note that it is ok to have a right brace in a string or quoted
    // string.
    s = MessageFormat.format("new {0}() '{..'}", new Object[] { "bob" });
    harness.check(s, "new bob() {..}");

    // As a practical matter, unterminated quotes are allowed for
    // compatibility.
    s = MessageFormat.format("new {0}() '{..}", new Object[] { "bob" });
    harness.check(s, "new bob() {..}");
  }

}
