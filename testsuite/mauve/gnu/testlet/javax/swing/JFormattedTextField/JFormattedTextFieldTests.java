// Tags: JDK1.4

// Copyright (C) 2005 Red Hat

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.


package gnu.testlet.javax.swing.JFormattedTextField;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.ParseException;

import javax.swing.JFormattedTextField;
import javax.swing.text.AbstractDocument;
import javax.swing.text.DefaultFormatter;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.MaskFormatter;


/**
 * Some checks for JFormattedTextField.
 */
public class JFormattedTextFieldTests implements Testlet
{
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    JFormattedTextField field = new JFormattedTextField();

    harness.checkPoint ("defaults");
    harness.check (((AbstractDocument)field.getDocument()).getDocumentFilter() == null);
    harness.check (field.getFormatterFactory() == null);
    harness.check (field.getFormatter() == null);


    harness.checkPoint ("implicit creation of formatter and factory");
    field.setValue("aBcDeFg");
    harness.check (((AbstractDocument)field.getDocument()).getDocumentFilter() != null);
    harness.check (field.getFormatterFactory().getClass(), 
                   DefaultFormatterFactory.class);
    harness.check (field.getFormatter().getClass(), DefaultFormatter.class);
    

    harness.checkPoint ("setting formatter changes the text");
    MaskFormatter mask = null;
    DefaultFormatter nullFormatter = new DefaultFormatter();
    try
      {
        mask = new MaskFormatter ("UUUUUUU");
      }
    catch (ParseException pe)
      {
      }
    DefaultFormatterFactory factory = 
      new DefaultFormatterFactory (mask, null, null, nullFormatter);
    harness.check (field.getText().equals("aBcDeFg"));
    field.setFormatterFactory(factory);
    harness.check (field.getText().equals("ABCDEFG"));

    harness.checkPoint ("field value going to null brings in nullFormatter");
    field.setValue(null);
    harness.check (field.getFormatter().getClass(), DefaultFormatter.class);        

    harness.checkPoint ("removing the DocumentFilter");
    field.getFormatter().uninstall();
    harness.check (((AbstractDocument)field.getDocument()).getDocumentFilter() == null);    
  }
}
