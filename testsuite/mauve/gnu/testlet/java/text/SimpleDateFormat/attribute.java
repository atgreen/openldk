/* attribute.java -- tests formatToCharacterIterator
   Copyright (C) 2003 Free Software Foundation

This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

// TAGS: JDK1.4
package gnu.testlet.java.text.SimpleDateFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;
import java.util.Date;
import java.util.Map;
import java.util.Set;
import java.util.Locale;
import java.util.TimeZone;


public class attribute implements Testlet {

  final private void test_Basic(TestHarness harness)
  {
    SimpleDateFormat format;

    try
      {
	format = new SimpleDateFormat("yyyy.MM.dd G 'at' HH:mm:ss z");
      }
    catch (Exception e)
      {
	harness.debug(e);
	harness.fail("Unexpected exception " + e);
	return;
      }
    
    harness.checkPoint("null argument");
    try
      {
	format.formatToCharacterIterator(null);
	harness.debug("It should have thrown an exception here");
	harness.check(false);
      }
    catch (NullPointerException _)
      {
	harness.check(true); 
      }
    catch (Exception e)
      {
  	harness.debug(e);
	harness.fail("Unexpected exception " + e);
      }

    harness.checkPoint("Illegal arguments");
    try
      {
	format.formatToCharacterIterator("invalid object");
	harness.check(false);
      }
    catch (IllegalArgumentException _)
      {
	harness.check(true);
      }
    catch (Exception e)
      {
	harness.debug(e);
	harness.check(false, "unexpected exception");
      }
  }
  
  public void test(TestHarness harness)
  {
    test_Basic(harness);
    test_Attributes(harness);
    test_FieldPos(harness);
  }

  final private void test_Attributes(TestHarness harness)
  { 
    harness.checkPoint("Attributes");
    try
      {
	Date date = new Date(1471228928L);
	SimpleDateFormat format2 = new SimpleDateFormat("yyyy.MM.dd hh:kk:mm:ss 'zone' zzzz", Locale.UK);
	format2.setTimeZone(TimeZone.getTimeZone("UTC"));
	AttributedCharacterIterator iterator = 
	  format2.formatToCharacterIterator(date);
	
	//Needed a larger range since time zones can have extended formats
	//i.e. 'Coordinated Universal Time' or UTC, GMT, etc.
	int[] range = new int[]
	  { 4, 5, 7, 8, 10, 11, 13, 14, 16, 17, 19, 20, 22, 28, 70};
	Object[] attrs = new Object[] {
	  DateFormat.Field.YEAR, null, DateFormat.Field.MONTH, null, DateFormat.Field.DAY_OF_MONTH,
	  null, DateFormat.Field.HOUR1, null, DateFormat.Field.HOUR_OF_DAY1, null,
	  DateFormat.Field.MINUTE, null, DateFormat.Field.SECOND,
	  null, DateFormat.Field.TIME_ZONE, null, null};
	int i, j;
	char c; 

	harness.debug("Date " + iteratorToString(iterator) + " length=" + iteratorToString(iterator).length());
	for (c = iterator.first(), i = 0, j = 0; c != CharacterIterator.DONE; j++, c = iterator.next())
	  {
	    if (range[i] == j)
	      i++;
	    if (attrs[i] != null)
	      {
		Map m = iterator.getAttributes();
		Set s = m.keySet();

		harness.debug("Position " + j);
		harness.check(s.size(), 1);
		if (s.size() != 0)
		  harness.check(s.iterator().next(), attrs[i]);
	      }
	    else
	      {
		harness.check(iterator.getAttributes().size(), 0);
	      }
	  }
      }
    catch (Exception e)
      {
	harness.debug(e);
	harness.check(false);
      }
  }

  final private void test_FieldPos(TestHarness harness)
  {
    harness.checkPoint("Field position");
    try
      {
	SimpleDateFormat format = new SimpleDateFormat("yyyy.MM.dd hh:kk:mm:ss 'zone' zzzz");
	Date date = new Date();
	Format.Field[] fields = new Format.Field[] {
	  DateFormat.Field.YEAR,  DateFormat.Field.MONTH, DateFormat.Field.DAY_OF_MONTH,
	  DateFormat.Field.HOUR1, DateFormat.Field.HOUR_OF_DAY1, DateFormat.Field.MINUTE,
	  DateFormat.Field.SECOND
	};
	int[] begin = new int[] {
	  0, 5, 8, /***/  11, 14, 17, 20
	};
	int[] end = new int[] {
	  4, 7, 10, /***/ 13, 16, 19, 22
	};

	harness.debug(format.format(date));

	for (int i = 0; i < fields.length; i++)
	  {
	    FieldPosition pos = new FieldPosition(fields[i]);
	    StringBuffer output = new StringBuffer(25);
	    
	    format.format(date, output, pos);
	    
	    harness.check(pos.getBeginIndex(), begin[i]);
	    harness.check(pos.getEndIndex(), end[i]);
	  }
   }
    catch (Exception e)
      {
	harness.debug(e);
	harness.check(false);
      }
  }

  private String iteratorToString(CharacterIterator iterator)
  {
    StringBuffer sb = new StringBuffer(iterator.getEndIndex()-iterator.getBeginIndex());
    
    for(char c = iterator.first(); c != CharacterIterator.DONE; c = iterator.next()) {
      sb.append(c);
    }
    
    return sb.toString();
  }
}
