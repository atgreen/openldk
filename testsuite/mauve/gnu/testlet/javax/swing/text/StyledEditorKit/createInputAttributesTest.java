// Tags: JDK1.2

// Copyright (C) 2005 Red Hat

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

package gnu.testlet.javax.swing.text.StyledEditorKit;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.text.*;
import javax.swing.text.html.*;

/**
 * Checks if createInputAttributes clears the set
 * before adding the new Attributes.
 */
public class createInputAttributesTest extends StyledEditorKit implements Testlet
{
  public void test(TestHarness harness)
  {
        MutableAttributeSet mas = new SimpleAttributeSet();
        mas.addAttribute("A", "B");
	HTMLDocument h = new HTMLDocument();
        Element e = h.getDefaultRootElement();
        createInputAttributes(e, mas);
        harness.check(mas.getAttribute("A") == null);
  }
}
