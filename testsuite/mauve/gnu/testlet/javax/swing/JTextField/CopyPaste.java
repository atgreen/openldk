// Tags: JDK1.4

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.JTextField;

import javax.swing.JTextField;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the copy/paste functionality. Must work without GUI as well.
 */
public class CopyPaste implements Testlet
{

  public void test(TestHarness harness)
  {

    String pasteIt = "the string to paste";
    JTextField field = new JTextField();

    field.setText(pasteIt);

    harness.check(field.getText(), pasteIt, "get/setText");
    field.selectAll();
    field.copy();

    JTextField field2 = new JTextField();
    field2.paste();

    harness.check(field2.getText(), pasteIt, "paste");

    field2.paste();
    harness.check(field2.getText(), pasteIt + pasteIt,"subsequent paste");
  }

}
