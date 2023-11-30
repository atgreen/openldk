// Tags: not-a-test 

// Copyright (C) 2005 Audrius Meskauskas <audriusa@bluewin.ch>

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

import java.io.PrintStream;
import java.util.Enumeration;

import javax.swing.text.AttributeSet;
import javax.swing.text.html.parser.Element;
import javax.swing.text.html.parser.TagElement;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ParserTest
  extends gnu.javax.swing.text.html.parser.support.Parser
{
  PrintStream out = System.out;
  StringBuffer errors = new StringBuffer();

  public ParserTest()
  {
    super(gnu.javax.swing.text.html.parser.HTML_401F.getInstance());
  }

  public static void main(String[] args)
  {
    String sx;
    sx =
      "<html><head></head><body><table>< tbody><tr ><   td >C_0_0< td>C_0_1<   td >C_0_2<   /td  >< td  >C_0_3<td>C_0_4<   /td></tr ></tbody></table></body></html>";
    try
      {
        System.out.println(sx);

        ParserTest t = new ParserTest();
        t.parse(new java.io.StringReader(sx));
        System.out.println("\nErrors:");
        System.out.println(t.errors);
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
      }
  }

  protected void handleComment(char[] parm1)
  {
    out.print("{" + new String(parm1) + "}");
  }

  protected void handleEOFInComment()
  {
    out.print(" [EOF in comment] ");
  }

  protected void handleEmptyTag(TagElement tag)
                         throws javax.swing.text.ChangedCharSetException
  {
    out.print("<" + tag);

    javax.swing.text.AttributeSet atts = getAttributes();
    dumpAttributes(atts);
    out.print("/>");
  }

  protected void handleEndTag(TagElement tag)
  {
    out.print("</" + tag + "> ");
  }

  protected void handleError(int line, String message)
  {
    errors.append(message);
    errors.append('\n');
  }

  protected void handleStartTag(TagElement tag)
  {
    out.print("<" + tag);

    javax.swing.text.AttributeSet atts = getAttributes();
    dumpAttributes(atts);
    out.print('>');
  }

  protected void handleText(char[] parm1)
  {
    out.print("'" + new String(parm1) + "'");
  }

  protected void handleTitle(char[] parm1)
  {
    out.print(" [ Title: " + new String(parm1) + "] ");
  }

  protected void markFirstTime(Element element)
  {
    out.print("(1:" + element + ")");
  }

  private void dumpAttributes(AttributeSet atts)
  {
    Enumeration e = atts.getAttributeNames();
    while (e.hasMoreElements())
      {
        String a = e.nextElement().toString();
        String v = (String) atts.getAttribute(a);
        out.print(" " + a + "='" + v + "'");
      }
  }
}
