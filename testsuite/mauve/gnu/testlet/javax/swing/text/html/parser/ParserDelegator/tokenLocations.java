// Tags: JDK1.2

// Copyright (C) 2005 Audrius Meskauskas, AudriusA@Bluewin.ch

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

import javax.swing.text.MutableAttributeSet;
import javax.swing.text.html.HTML;


/**
 * Checking the token positons that must be available for the callback.
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class tokenLocations
  extends parsingTester
  implements Testlet
{
  public void handleSimpleTag(HTML.Tag tag, MutableAttributeSet attributes,
                              int position
                             )
  {
    if (tag.toString().equals("#pcdata"))
      return;
    out.append("<" + tag + "[" + position + "]");
    dumpAttributes(attributes);
    out.append("/>");
  }

  public void handleStartTag(HTML.Tag tag, MutableAttributeSet attributes,
                             int position
                            )
  {
    if (tag.toString().equalsIgnoreCase("tbody"))
      return;
    out.append("<" + tag + "[" + position + "]");
    dumpAttributes(attributes);
    out.append('>');
  }

  public void handleText(char chars[], int position)
  {
    out.append("'" + new String(chars) + "[" + position + "]'");
  }

  public void handleEndTag(HTML.Tag tag, int position)
  {
    if (tag.toString().equalsIgnoreCase("tbody"))
      return;
    out.append("</" + tag + ">");
  }

  public void handleComment(char parm1[], int position)
  {
    out.append("{" + new String(parm1) + "[" + position + "]}");
  }

  public void testHTMLParsing()
  {
    hideImplied = true;

    // 0123456789012345678901234567890
    verify("a<!-- comment -->b<!-- comment2 -->",
           "<html[0]><head[0]></head><body[0]>'a[0]'{ comment [1]}'b[17]'" +
           "{ comment2 [18]}</body></html>", "comment"
          );

    // 0123456789012345678901234567890
    verify("<table><tr><td>a<td>b<td></tr>",
           "<html[0]><head[0]></head><body[0]><table[0]>" +
           "<tr[7]><td[11]>'a[15]'</td><td[16]>'b[20]'</td>" +
           "<td[21]></td></tr></table></body>" + "</html>", "table"
          );

    // 012345678901234567
    verify("<p>b<p>c<p>d",
           "<html[0]><head[0]></head><body[0]><p[0]>'b[3]'</p><p[4]>'c[7]'" +
           "</p><p[8]>'d[11]'</p></body></html>", "paragraphs"
          );

    // Test SGML insertion
    verify("<! the sgml construct >sgml",
       "<html[23]><head[23]></head><body[23]>"+
       "'sgml[23]'</body></html>","SGML insertion");

  }

  public void test(TestHarness a_harness)
  {
    super.test(a_harness);
  }
}
