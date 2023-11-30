// Tags: JDK1.2

// Copyright (C) 2006 Audrius Meskauskas, Audrius.Meskauskas@Spectraseis.org

// // This file is part of Mauve.

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

package gnu.testlet.javax.swing.text.html.parser.ParserDelegator;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.IOException;
import java.io.StringReader;

import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.TreeSet;

import javax.swing.text.AttributeSet;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.HTMLEditorKit.ParserCallback;
import javax.swing.text.html.parser.ParserDelegator;


/**
 * Sample HTML parsing. This test may fail on some 1.3 implementations,
 * revealing bugs that were later fixed in 1.4.
 * 
 * @author Audrius Meskauskas (Audrius.Meskauskas@Spectraseis.org)
 */
public class SimpleParsing
  extends HTMLEditorKit.ParserCallback
  implements Testlet
{
  StringBuffer out = new StringBuffer();
  AttributeSet atts = new SimpleAttributeSet();
  TestHarness harness;

  public void handleComment(char parm1[], int position)
  {
    out.append("{" + new String(parm1) + "}");
  }

  public void handleEndTag(HTML.Tag tag, int position)
  {
    if (tag.toString().equals("tbody"))
      return;
    out.append("</" + tag + ">");
  }

  public void handleSimpleTag(HTML.Tag tag, MutableAttributeSet attributes,
                              int position
                             )
  {
    if (tag.toString().equals("#pcdata"))
      return;
    out.append("<" + tag);
    dumpAttributes(attributes);
    out.append("/>");
  }

  public void handleStartTag(HTML.Tag tag, MutableAttributeSet attributes,
                             int position
                            )
  {
    if (tag.toString().equals("tbody"))
      return;
    out.append("<" + tag);
    dumpAttributes(attributes);
    out.append('>');
  }

  public void handleText(char chars[], int position)
  {
    out.append("'" + new String(chars) + "'");
  }

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    testHTMLParsing();
  }

  public void testHTMLParsing()
  {
    // Test entities.
    verify("eqdec: &#61; ampnamed: &amp;",
           "<html _implied_='true'><head _implied_='true'></head>" +
           "<body _implied_='true'>'eqdec: = ampnamed: &'</body></html>",
           "Named and numeric entities"
          );

    // Test unclosed tags.
    verify("<hr id = 1 class = c<hr id=2>",
           "<html _implied_='true'><head _implied_='true'></head>" +
           "<body _implied_='true'><hr class='c' id='1'/><hr id='2'/>" +
           "</body></html>", "Error tolerance (unclosed tags)"
          );

    // Test valid attributes.
    verify("<hr id='i' TITLE=\"tit\" clAss=cl><hr><hr id = 2>",
           "<html _implied_='true'><head _implied_='true'>" +
           "</head><body _implied_='true'><hr class='cl' id='i' title='tit'/>" +
           "<hr/><hr id='2'/></body></html>", "Attributes"
          );

   // Test unknown attribute without value.
   verify("<hr audrius title=\"tit\">",
            "<html _implied_='true'><head _implied_='true'>"+
            "</head><body _implied_='true'><hr audrius='#DEFAULT' "+
            "title='tit'/></body></html>",
            "surely unknown attribute 'audrius' without value"
           );

    // Table tests:
    verify("<table><tr><td>a</td><tr><td>a</td>",
           "<html _implied_='true'><head _implied_='true'></head>" +
           "<body _implied_='true'><table><tr><td>'a'</td></tr><tr>" +
           "<td>'a'</td></tr></table></body></html>",
           "Table with implied row closing tags 1"
          );

    verify("<table><tr><td>a<td>b<tr><td>a<td>b<td>c",
           "<html _implied_='true'><head _implied_='true'></head>" +
           "<body _implied_='true'><table><tr><td>'a'</td><td>'b'</td>" +
           "</tr><tr><td>'a'</td><td>'b'</td><td>'c'</td></tr></table>" +
           "</body></html>", "Table with implied row closing tags 2"
          );

    verify("<table><tr><td>a<td>b<td>c</tr>",
           "<html _implied_='true'><head _implied_='true'>" +
           "</head><body _implied_='true'><table><tr><td>'a'</td>" +
           "<td>'b'</td><td>'c'</td></tr></table></body></html>",
           "Table with implied row and column closing tags"
          );

    // Test table content model.
    verify("<table>a</table>",
           "<html _implied_='true'><head _implied_='true'></head>" +
           "<body _implied_='true'><table><tr _implied_='true'>" +
           "<td _implied_='true'>'a'</td></tr></table></body></html>",
           "Table with implied tags"
          );

    // Test table content model.
    verify("<table><caption>cap</caption>a</table>",
           "<html _implied_='true'><head _implied_='true'>" +
           "</head><body _implied_='true'><table><caption>'cap'" +
           "</caption><tr _implied_='true'><td _implied_='true'>'a'</td>" +
           "</tr></table></body></html>", "Table with caption"
          );

    // Test typical table.
    verify("<TABLE><tr><td>x</td><td>y</td><td>z</td></table>",
           "<html _implied_='true'><head _implied_='true'>" +
           "</head><body _implied_='true'><table><tr><td>'x'</td>" +
           "<td>'y'</td><td>'z'</td></tr></table></body></html>", "Simple table"
          );

   // Test nested table.
   verify("<table><tr><td><table>nested</table>x</td><td>"+
           "y</td><td>z</td></table>",
           "<html _implied_='true'><head _implied_='true'></head>"+
           "<body _implied_='true'><table>"+
           "<tr><td><table><tr _implied_='true'>"+
           "<td _implied_='true'>'nested'</td></tr></table>'x'"+
           "</td><td>'y'</td><td>'z'</td></tr></table></body></html>",
           "nested table"
          );

    // Test simple nested list.
    verify("<ul><li>a</li><ul><li>na</li><li>nb</li></ul><li>b</li></ul>",
           "<html _implied_='true'><head _implied_='true'>" +
           "</head><body _implied_='true'><ul><li>'a'</li><ul><li>'na'</li>" +
           "<li>'nb'</li></ul><li>'b'</li></ul></body></html>", "Nested list"
          );

    // Test simple non-nested list.
    verify("<ul><li>a</li><lI>na</li><li>nb</li><li>b</li></Ul>",
           "<html _implied_='true'><head _implied_='true'>" +
           "</head><body _implied_='true'><ul><li>'a'</li><li>'na'</li>" +
           "<li>'nb'</li><li>'b'</li></ul></body></html>", "Simple list"
          );

    // Test list without closing tags (obsolete list form).
    verify("<ul><li>a<li>na<li>nb<li>b</ul>",
           "<html _implied_='true'><head _implied_='true'></head>" +
           "<body _implied_='true'><ul><li>'a'</li><li>'na'</li><li>'nb'</li>" +
           "<li>'b'</li></ul></body></html>", "List with implied closing tags"
          );

    // Test list without closing tags (obsolete list form).
    verify("<ul><li>a<ul><li>na<li>nb</ul><li>b</ul>",
           "<html _implied_='true'><head _implied_='true'></head>" +
           "<body _implied_='true'><ul><li>'a'<ul><li>'na'</li><li>'nb'</li>" +
           "</ul></li><li>'b'</li></ul></body></html>",
           "List with implied closing tags"
          );

    // Test html no head no body.
    verify("<html>text</html>",
           "<html><head _implied_='true'></head><body _implied_='true'>" +
           "'text'</body></html>", "Global document content model 1"
          );

    // Test head only.
    verify("<head></head>text",
           "<html _implied_='true'><head></head><body _implied_='true'>" +
           "'text'</body></html>", "Global document content model 2"
          );

    // Test head and body.
    verify("<head><title>ti</title></head><body>text",
           "<html _implied_='true'><head><title>'ti'</title></head><body>" +
           "'text'</body></html>", "Global document content model 3"
          );

    // Test title and text.
    verify("<title>title</title>text",
           "<html _implied_='true'><head _implied_='true'><title>'title'" +
           "</title></head><body _implied_='true'>'text'</body></html>",
           "Global document content model 4"
          );

    // Test html only.
    verify("<html>text</html>",
           "<html><head _implied_='true'></head><body _implied_='true'>" +
           "'text'</body></html>", "Global document content model 5"
          );

    // Test body only.
    verify("<body>text</body>",
           "<html _implied_='true'><head _implied_='true'></head><body>" +
           "'text'</body></html>", "Global document content model 6"
          );

    // Test definition list, obsolete.
    verify("<dl><dt>ha<dd>a<dt>hb<dd>b",
           "<html _implied_='true'><head _implied_='true'></head>" +
           "<body _implied_='true'><dl><dt>'ha'</dt><dd>'a'</dd><dt>" +
           "'hb'</dt><dd>'b'</dd></dl></body></html>",
           "Definition list without closing tags"
          );

    // Test definition list.
    verify("<html><head></head><body><dl><dt>'ha'</dt><dd>'a'</dd>" +
           "<dt>'hb'</dt><dd>'b'</dd></dl></body></html>",
           "<html><head></head><body><dl><dt>''ha''</dt><dd>''a''</dd><dt>" +
           "''hb''</dt><dd>''b''</dd></dl></body></html>", "Definition list"
          );

    // Test paragraphs.
    verify("<p>b<p>c<p>d",
           "<html _implied_='true'><head _implied_='true'></head>" +
           "<body _implied_='true'><p>'b'</p><p>'c'</p><p>'d'</p>" +
           "</body></html>", "Paragraphs without closing tags"
          );

    // Test paragraphs.
    verify("<p>'b'</p><p>'c'</p><p>'d'</p>",
           "<html _implied_='true'><head _implied_='true'></head>" +
           "<body _implied_='true'><p>''b''</p><p>''c''</p><p>''d''" +
           "</p></body></html>", "Paragraphs"
          );

    // Test select obsolete.
    verify("<form><select><option value='hi'>a<option selected>b" +
           "<option>normal",
           "<html _implied_='true'><head _implied_='true'></head>" +
           "<body _implied_='true'><form><select><option value='hi'>" +
           "'a'</option><option selected='selected'>'b'</option>" +
           "<option>'normal'</option></select></form></body></html>",
           "Forms, options without closing tags, bug in 1.3, fixed in 1.4"
          );

    // Test select current.
    verify("<form><select><option>'a'</option><option " +
           "SeLeCtEd='selected'>'b'</option></select></form>",
           "<html _implied_='true'><head _implied_='true'>" +
           "</head><body _implied_='true'><form><select><option>" +
           "''a''</option><option selected='selected'>''b''</option>" +
           "</select></form></body></html>", "Forms, bug in 1.3, fixed in 1.4"
          );
  }

  public String verify(String html, String expected, String about)
  {
    out.setLength(0);

    HTMLEditorKit.ParserCallback callback = this;
    ParserDelegator delegator = new ParserDelegator();
    try
      {
      delegator.parse(new StringReader(html), callback, true);
      }
    catch (IOException ex)
      {
      harness.fail("Unexpected exception " + ex.getMessage() +
                   " while parsing " + html
                  );
      }

    String result = out.toString();

    harness.check(result, expected, about);
    return result;
  }

  public boolean hideImplied = false;

  protected void dumpAttributes(AttributeSet atts)
  {
    Enumeration enumeration = atts.getAttributeNames();

    // Sort them to ensure the same order every time:
    TreeSet t =
      new TreeSet(new Comparator()
          {
          public int compare(Object a, Object b)
          {
            return a.toString().compareTo(b.toString());
          }
          }
                 );
    while (enumeration.hasMoreElements())
      t.add(enumeration.nextElement());

    Iterator iter = t.iterator();

    while (iter.hasNext())
      {
      Object a = iter.next();
      Object av = atts.getAttribute(a);
      if (hideImplied &&
          a.toString().equalsIgnoreCase(ParserCallback.IMPLIED.toString())
         )
        continue;

      String v = av != null ? av.toString() : "~null";
      out.append(" " + a + "='" + v + "'");
      }
  }
}
