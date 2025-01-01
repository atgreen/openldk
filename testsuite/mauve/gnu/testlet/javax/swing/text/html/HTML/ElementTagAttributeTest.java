// Tags: JDK1.2

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.text.html.HTML;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

import javax.swing.text.AttributeSet;
import javax.swing.text.Element;
import javax.swing.text.ElementIterator;
import javax.swing.text.StyleConstants;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.parser.ParserDelegator;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;


/**
 * Test that the HTML.Tags are stored as StyleConstants.NameAttribute
 * in the Elements attributes
 * @author Lillian Angel (langel@redhat.com)
 */
public class ElementTagAttributeTest implements Testlet
{
  TestHarness h;

  public void test(TestHarness harness)
  {
    h = harness;
    try
      {
        testAttrs();
      }
    catch (Exception e)
      {
        h.fail("Exception caught");
      }
  }

  public void testAttrs() throws Exception
  {
    URL url = new URL("http://www.redhat.com");
    URLConnection connection = url.openConnection();
    InputStream is = connection.getInputStream();
    InputStreamReader isr = new InputStreamReader(is);
    BufferedReader br = new BufferedReader(isr);
    HTMLEditorKit htmlKit = new HTMLEditorKit();
    HTMLDocument htmlDoc = (HTMLDocument) htmlKit.createDefaultDocument();
    HTMLEditorKit.Parser parser = new ParserDelegator();
    HTMLEditorKit.ParserCallback callback = htmlDoc.getReader(0);
    parser.parse(br, callback, true);

    ElementIterator iterator = new ElementIterator(htmlDoc);
    Element element;
    while ((element = iterator.next()) != null)
      {
        AttributeSet attributes = element.getAttributes();
        Object name = attributes.getAttribute(StyleConstants.NameAttribute);
        h.check((name instanceof HTML.Tag),
                true,
                "HTML.Tag is not"
                    + " stored as StyleConstants.NameAttribute in the elements attributes");
      }
  }
}
