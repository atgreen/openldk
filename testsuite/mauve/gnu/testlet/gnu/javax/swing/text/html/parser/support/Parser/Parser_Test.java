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

import java.io.StringReader;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.TreeSet;

import javax.swing.text.AttributeSet;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.parser.ParserDelegator;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Parser_Test
  extends HTMLEditorKit.ParserCallback
{
  public boolean hideImplied = true;
  protected StringBuffer out = new StringBuffer();
  AttributeSet atts = new SimpleAttributeSet();

  public void generate(String x, String comment)
                throws Exception
  {
    String prolog = "<html><head></head><body>";
    String epilog = "</body></html>";
    String html = x; // prolog+x+epilog;
    System.out.println("// Test " + comment + ".");
    System.out.println("v.verify(\"" + html + "\",\n  \"" + verify(html, null) +
                       "\");"
                      );
  }

  public void handleComment(char[] parm1, int position)
  {
    out.append("{" + new String(parm1) + "}");
  }

  public void handleEndTag(HTML.Tag tag, int position)
  {
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
    out.append("<" + tag);
    dumpAttributes(attributes);
    out.append('>');
  }

  public void handleText(char[] chars, int position)
  {
    out.append("'" + new String(chars) + "'");
  }

  public String verify(String html, String trace)
                throws Exception
  {
    out.setLength(0);

    HTMLEditorKit.ParserCallback callback = this;
    ParserDelegator delegator = new ParserDelegator();
    delegator.parse(new StringReader(html), callback, true);

    String ou = out.toString();
    if (trace != null)
      {
        if (!ou.equals(trace))
          {
            System.err.println("Unable to parse '" + html + "':");
            System.err.println("    expected: '" + trace + "',");
            System.out.println("    returned: '" + ou + "'.");
            throw new Exception("'" + html + "' -> '" + ou + "' expected '" +
                                trace + "'"
                               );
          }
      }
    return ou;
  }

  protected void dumpAttributes(AttributeSet atts)
  {
    Enumeration e = atts.getAttributeNames();

    // Sort them to ensure the same order every time:
    TreeSet t = new TreeSet();
    while (e.hasMoreElements())
      t.add(e.nextElement().toString());

    Iterator iter = t.iterator();

    while (iter.hasNext())
      {
        String a = iter.next().toString();

        if (hideImplied)
          if (a.equals("_implied_"))
            continue;

        String v = atts.getAttribute(a).toString();
        out.append(" " + a + "='" + v + "'");
      }
  }
}
