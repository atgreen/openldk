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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.JTextField;

import java.beans.PropertyChangeEvent;

import javax.swing.JTextField;
import javax.swing.text.Document;
import javax.swing.plaf.basic.BasicTextFieldUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks if the setDocument method sets the "filterNewLines" property and if
 * it does so through a PropertyChangeEvent.
 */
public class setDocument implements Testlet
{
  static TestHarness h2;

  class TestUI extends BasicTextFieldUI
  {
    protected void propertyChange(PropertyChangeEvent evt)
    {
      Document doc = ((JTextField)getComponent()).getDocument();
      h2.check (doc.getProperty("filterNewlines"), Boolean.TRUE);
      super.propertyChange(evt);
      h2.check (doc.getProperty("filterNewlines"), Boolean.TRUE);
    }
  }

  static class TestTextField extends JTextField
  {
    public void setDocument(Document doc)
    {
      h2.check (doc.getProperty("filterNewlines") == null);
      super.setDocument(doc);
      h2.check (doc.getProperty("filterNewlines") != null);
    }

    public Document createDefaultModel()
    {
      return super.createDefaultModel();
    }
  }

  /**
   * Starts the testrun.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    h2 = harness;
    TestTextField tf = new TestTextField();
    tf.setUI(new TestUI());
    Document doc = tf.createDefaultModel();
    tf.setDocument(doc);
  }

}
