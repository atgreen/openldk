// Tags: JDK1.2

// Copyright (C) 2005 Red Hat.

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

package gnu.testlet.javax.swing.text.MaskFormatter;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.text.ParseException;
import javax.swing.text.MaskFormatter;

/**
 * Several tests related to MaskFormatter.
 */
public class MaskFormatterTest
  implements Testlet
{
  
  public void test(TestHarness harness)
  {
    MaskFormatter formatter = null;
    try 
      { 
        formatter = new MaskFormatter("RE'*B**KS");        
        // Default value checks
        harness.checkPoint("defaults");
        harness.check (formatter.getValueContainsLiteralCharacters());
        harness.check (!formatter.getAllowsInvalid());
        harness.check (formatter.getPlaceholder() == null);
        harness.check (formatter.getPlaceholderCharacter() == ' ');
        harness.check (formatter.getValidCharacters() == null);
        harness.check (formatter.getInvalidCharacters() == null);


        // Checks to see whether the appropriate characters are being used to pad
        harness.checkPoint("padding");
        formatter.setPlaceholder("MMMMMMMMM");
        formatter.setPlaceholderCharacter('$');
        harness.check (formatter.valueToString("RE"),"RE*BMMKS");
        formatter.setPlaceholder("8");
        harness.check (formatter.valueToString("RE"),"RE*B$$KS");
        formatter.setPlaceholder("12345");
        harness.check (formatter.valueToString("RE"),"RE*B5$KS");
        

        // Checks to see if valid output is produced
        harness.checkPoint("valid output");
        formatter.setMask("(###) ###-####");
        harness.check (formatter.valueToString("(555) 807-9090"),"(555) 807-9090");
        harness.check (formatter.stringToValue("(555) 807-9090"),"(555) 807-9090");
        formatter.setValueContainsLiteralCharacters(false);
        harness.check (formatter.stringToValue("(555) 807-9090"),"5558079090");
        boolean exception = false;
        try
          {
            harness.check (formatter.valueToString("(555) 807-9090"),"(555) 807-9090");
          }
        catch (ParseException pe)
          {
            exception = true;
          }
        harness.check (exception);                  
        
        // Checks to see if valid/invalid character sets work
        formatter = new MaskFormatter("T##'FA");        
        formatter.setInvalidCharacters("T");
        formatter.setValidCharacters("4");
        harness.check(formatter.valueToString("T44F4"), "T44F4");
        harness.check(formatter.stringToValue("T44F4"), "T44F4");
        exception = false;
        try
          {
            formatter.valueToString("T33F3");
          }
        catch (ParseException pe2)
          {
            exception = true;
          }
        harness.check(exception);
      }
    catch (java.text.ParseException ex) 
      {
        harness.fail ("an exception was thrown that shouldn't have been");
      }    
  }
}
