/* getDocumentProperties.java -- Tests AbstractDocument.getDocumentProperties()
   Copyright (C) 2006 Roman Kennke <roman@kennke.org>
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
// Uses: TestAbstractDocument

package gnu.testlet.javax.swing.text.AbstractDocument;

import java.util.Dictionary;
import java.util.Enumeration;

import javax.swing.text.AbstractDocument;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getDocumentProperties implements Testlet
{

  public void test(TestHarness harness)
  {
    testDefault(harness);

  }

  private void testDefault(TestHarness h)
  {
    AbstractDocument doc = new TestAbstractDocument();
    Dictionary props = doc.getDocumentProperties();
    Enumeration keys = props.keys();
    while (keys.hasMoreElements())
      {
        Object key = keys.nextElement();
      }
    h.check(props.size(), 1);
    h.check(props.get("i18n"), Boolean.FALSE);
  }

}
