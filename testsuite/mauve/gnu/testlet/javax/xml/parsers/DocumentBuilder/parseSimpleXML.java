// Tags: JDK1.2
// Uses: Verifyer

// Copyright (C) 2004, 2005 Audrius Meskauskas <audriusa@bluewin.ch>

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


package gnu.testlet.javax.xml.parsers.DocumentBuilder;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * A simple XML parsing test.
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class parseSimpleXML
  extends Verifyer
  implements Testlet
{
  public void test(TestHarness a_harness)
  {
    harness = a_harness;

    String head = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
    verify(head +
           "<xa ap1=\"apv1\" ap2=\"apv2\"><b/><c><a ap1=\"n\"></a></c></xa>",
           "#document xa ap1='apv1'ap2='apv2' b c a ap1='n'", "simple xml"
          );

    verify(head + "<xa ap1=\"apv1\" ap2=\"apv2\">t0<b/>t1<c><a ap1=\"n\">" +
           "</a></c>te</xa>",
           "#document xa ap1='apv1'ap2='apv2' #text  = 't0' b #text  = 't1' c" +
           " a ap1='n' #text  = 'te'", "simple xml with text nodes"
          );

    verify(head + "<a><!--explain--></a>", "#document a #comment  = 'explain'",
           "comment"
          );

    verify(head + "<a><!--e1--><!--e2--></a>",
           "#document a #comment  = 'e1' #comment  = 'e2'",
           "subsequent comments"
          );

    verify(head + "<a xmlns:ans=\"www.lithuania.lt\"><ans:b></ans:b></a>",
           "#document a xmlns:ans='www.lithuania.lt' www.lithuania.lt:ans:b",
           "explicit namespace"
          );

    verify(head + "<a xmlns=\"www.lithuania.lt\"><b></b></a>",
           "#document www.lithuania.lt:a xmlns='www.lithuania.lt' www.lithuania.lt:b",
           "default namespace"
          );

    verify(head +
           "<a xmlns=\"www.lithuania.lt\" xmlns:ans=\"www.gnu.org\"><b><ans:c>" +
           "</ans:c></b></a>",
           "#document www.lithuania.lt:a " +
           "xmlns='www.lithuania.lt'xmlns:ans='www.gnu.org' " +
           "www.lithuania.lt:b www.gnu.org:ans:c", "mixed namespaces"
          );

    verify("<xa ap1=\"apv1\" ap2=\"apv2\"><b/><c><a ap1=\"n\"></a></c></xa>",
           "#document xa ap1='apv1'ap2='apv2' b c a ap1='n'",
           "missing xml header"
          );
  }
}
