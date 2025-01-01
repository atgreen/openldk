/* getDocumentProperties.java -- Tests getDocumentProperties()
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.2

package gnu.testlet.javax.swing.text.PlainDocument;

import java.util.Dictionary;

import javax.swing.text.PlainDocument;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests getDocumentProperties().
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class getDocumentProperties implements Testlet
{

  public void test(TestHarness harness)
  {
    testDefault(harness);

  }

  private void testDefault(TestHarness h)
  {
    PlainDocument doc = new PlainDocument();
    Dictionary props = doc.getDocumentProperties();
    h.check(props.size(), 2);
    // This property is inherited from AbstractDocument.
    h.check(props.get("i18n"), Boolean.FALSE);
    h.check(props.get("tabSize"), new Integer(8));
  }
}
