// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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

package gnu.testlet.javax.swing.JTextField;

import javax.swing.JTextField;
import javax.swing.text.Document;
import javax.swing.text.PlainDocument;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks if the method createDefaultModel in JTextField returns the correct
 * value. It must be a document of type javax.swing.text.PlainDocument.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class createDefaultModel implements Testlet
{

  /**
   * A testclass that overrides the protected method.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  static class TestTextField extends JTextField
  {
    /**
     * Overridden to make this method publicly accessible.
     *
     * @return the created document
     */
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
    TestTextField tf = new TestTextField();
    Document doc = tf.createDefaultModel();
    harness.check(doc.getClass(), PlainDocument.class);
    harness.check(doc.getProperty("filterNewlines"), null);
  }

}
