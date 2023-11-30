// Tags: JDK1.4

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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


package gnu.testlet.java.text.DecimalFormat;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.AttributedCharacterIterator;
import java.text.DecimalFormat;
import java.text.NumberFormat.Field;
import java.util.Iterator;
import java.util.Set;

/**
 * Some checks for the formatToCharacterIterator() method in the
 * {@link DecimalFormat} class.
 */
public class formatToCharacterIterator
    implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    DecimalFormat f1 = new DecimalFormat();

    // check null argument
    boolean pass = false;
    try
      {
        f1.formatToCharacterIterator(null);
      }
    catch (NullPointerException e)
      {
        pass = true;
      }
    harness.check(pass);

    // check non-numeric argument
    pass = false;
    try
      {
        f1.formatToCharacterIterator("Not a number");
      }
    catch (IllegalArgumentException e)
      {
        pass = true;
      }
    harness.check(pass);
    
    harness.checkPoint("Check for attributes after a valid parse");
    
    DecimalFormat f2 = new DecimalFormat("0.##;-0.##");
    
    // result is "-1234.56"
    AttributedCharacterIterator chIter =
      f2.formatToCharacterIterator(Double.valueOf(-1234.56));
    
    Set _keys = chIter.getAllAttributeKeys();
    
    // It seems that we don't get always the same order in the results
    // but the values need to be the ones written later.
    // This test should be completed checking even the start/end of each field
    boolean pass1 = false;
    boolean pass2 = false;
    boolean pass3 = false;
    boolean pass4 = false;
    
    for (Iterator i = _keys.iterator(); i.hasNext();)
      {
        //harness.debug("field: " + i.next());
        Field field = (Field) i.next();
        if (field.equals(Field.INTEGER)) pass1 = true;
        if (field.equals(Field.FRACTION)) pass2 = true;
        if (field.equals(Field.DECIMAL_SEPARATOR)) pass3 = true;
        if (field.equals(Field.SIGN)) pass4 = true;
      }
    
    harness.check(pass1 && pass2 && pass3 && pass4);
  }
}
