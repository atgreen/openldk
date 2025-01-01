/* setSelectionStart.java 
   Copyright (C) 2006 Red Hat
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

// Tags: JDK1.1

package gnu.testlet.java.awt.TextComponent;

import java.awt.TextComponent;
import java.awt.TextField;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class setSelectionStart implements Testlet
{

  public void test(TestHarness harness)
  {
    TextComponent textComp = new TextField();

    // Check that default value is correct.
    harness.check(textComp.getSelectionStart(), 0);
    harness.check(textComp.getSelectionEnd(), 0);
    
    // Check behaviour when setting text.
    textComp.setText("This is some text.");
    harness.check(textComp.getSelectionStart(), 0);
    harness.check(textComp.getSelectionEnd(), 0);
    
    // Check behaviour when start < 0.
    textComp.setSelectionStart(-6);
    harness.check(textComp.getSelectionStart(), 0);
    harness.check(textComp.getSelectionEnd(), 0);
    
    // Check behaviour when start = end
    textComp.setSelectionStart(0);
    harness.check(textComp.getSelectionStart(), 0);
    harness.check(textComp.getSelectionEnd(), 0);
    
    // Check behaviour when start > end
    textComp.setSelectionStart(13);
    harness.check(textComp.getSelectionStart(), 13);
    harness.check(textComp.getSelectionEnd(), 13);
    
    // Check behaviour when start < end
    textComp.setSelectionStart(9);
    harness.check(textComp.getSelectionStart(), 9);
    harness.check(textComp.getSelectionEnd(), 13);

  }

}
