// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <roman@kennke.org>

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

package gnu.testlet.javax.swing.text.StyleContext;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.text.StyleContext;
import javax.swing.text.SimpleAttributeSet;

public class NamedStyleSetResolveParent implements Testlet
{
  public void test(TestHarness h)
  {
    h.checkPoint("StyleContext.NamedStyle");
    StyleContext sc = new StyleContext();

    try
      {
        StyleContext.NamedStyle ns = sc.new NamedStyle("key", null);
        ns.setResolveParent(null);
        h.check(true);
      }
    catch (NullPointerException ex)
      {
        h.fail("StyledContext.NamedValue.setResolveParent() must accept null values");
      }
  }

}
