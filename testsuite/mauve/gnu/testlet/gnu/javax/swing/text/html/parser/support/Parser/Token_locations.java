// Tags: JDK1.2 GNU
// Uses: TestCase Parser_Test

// Copyright (C) 2005, 2006 Audrius Meskauskas <audriusa@bluewin.ch>

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

package gnu.testlet.gnu.javax.swing.text.html.parser.support.Parser;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.MutableAttributeSet;
import javax.swing.text.html.HTML;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Token_locations
  extends TestCase
  implements Testlet
{
  public void test(TestHarness harness)
  {
    h = harness;
    try
      {
        testHTMLParsing();
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        harness.fail("Exception: " + ex);
      }
  }

  public void testHTMLParsing()
                       throws Exception
  {
    Parser_Test v =
      new Parser_Test()
      {
        public void handleSimpleTag(HTML.Tag tag,
                                    MutableAttributeSet attributes, int position
                                   )
        {
          if (tag.toString().equals("#pcdata"))
            return;
          out.append("<" + tag + "[" + position + "]");
          dumpAttributes(attributes);
          out.append("/>");
        }

        public void handleStartTag(HTML.Tag tag,
                                   MutableAttributeSet attributes, int position
                                  )
        {
          if (tag.toString().equalsIgnoreCase("tbody"))
            return;
          out.append("<" + tag + "[" + position + "]");
          dumpAttributes(attributes);
          out.append('>');
        }

        public void handleText(char[] chars, int position)
        {
          out.append("'" + new String(chars) + "[" + position + "]'");
        }

        public void handleEndTag(HTML.Tag tag, int position)
        {
          if (tag.toString().equalsIgnoreCase("tbody"))
            return;
          out.append("</" + tag + "[" + position + "]>");
        }

        public void handleComment(char[] parm1, int position)
        {
          out.append("{" + new String(parm1) + "[" + position + "]}");
        }
      };

    v.hideImplied = true;

    // 0123456789012345678901234567890
    v.verify("<table><tr><td>a<td>b<td>c</tr>",
             "<html[0]><head[0]></head[0]><body[0]><table[0]>" +
             "<tr[7]><td[11]>'a[15]'</td[16]><td[16]>'b[20]'</td[21]>" +
             "<td[21]>'c[25]'</td[26]></tr[26]></table[26]></body[26]>" +
             "</html[26]>"
            );

    // 0123456789012345678901234567890
    v.verify("a<!-- comment -->b<!-- comment2 -->",
             "<html[0]><head[0]></head[0]><body[0]>'a[0]'{ comment [1]}'b[17]'" +
             "{ comment2 [18]}</body[18]></html[18]>"
            );

    // 012345678901234567
    v.verify("<p>b<p>c<p>d",
             "<html[0]><head[0]></head[0]><body[0]><p[0]>'b[3]'</p[4]><p[4]>'" +
             "c[7]'</p[8]><p[8]>'d[11]'</p[11]></body[11]></html[11]>"
            );

    // Test SGML
    v.verify("<! the sgml construct >sgml",
             "<html[23]><head[23]></head[23]><body[23]>" +
             "'sgml[23]'</body[23]></html[23]>"
            );
  }
}
