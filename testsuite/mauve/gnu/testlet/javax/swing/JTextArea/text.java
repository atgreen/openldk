// Tags: JDK1.2

// Copyright (C) 2004 Audrius Meskauskas <audriusa@bluewin.ch>

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


package gnu.testlet.javax.swing.JTextArea;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JTextArea;

/**
 * The basic text access functions that must also work when the
 * component is not displayed.
 * <p>@author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class text
  implements Testlet
{
  public void test(TestHarness harness)
  {
    JTextArea t1 = new JTextArea();

    t1.append("{a\nb\nc}");
    harness.check(t1.getLineCount(), 3, "getLineCount");
    harness.check(t1.getText(), "{a\nb\nc}", "simple getText");

    t1.setText("0123456789");
    t1.setSelectionStart(2);
    t1.setSelectionEnd(5);
    harness.check(t1.getSelectedText(),"234","getSelectedText");
    t1.replaceRange("replacement", 3, 5);
    harness.check(t1.getText(),"012replacement56789","replacement");
    t1.insert("insertion", 1);
    harness.check(t1.getText(),"0insertion12replacement56789","insertion");
    t1.setSelectionStart(0);
    t1.setSelectionEnd(1);
    t1.replaceSelection("selection");
    harness.check(t1.getText(),"selectioninsertion12replacement56789",
    "insertion");
  }
}
