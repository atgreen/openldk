// Tags: JDK1.4

// Copyright (C) 2004, 2006 Michael Koch <konqueror@gmx.de>

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

package gnu.testlet.javax.swing.text.DefaultEditorKit;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.Action;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.TextAction;

public class getActions
  implements Testlet
{
  private void checkForAction(TestHarness h, Action[] actions, String name)
  {
    boolean found = false;

    for (int i = 0 ; i < actions.length; ++i)
      if (((TextAction) actions[i]).getValue(Action.NAME).equals(name))
	{
	  found = true;
	  break;
	}

    if (found) {
    h.check(found, name);
    }
  }
  
  public void test(TestHarness h)
  {
    DefaultEditorKit editorKit = new DefaultEditorKit();
    Action[] actions = editorKit.getActions();

    h.check(actions != null, "actions != null");
    h.check(actions.length, 53, "number of actions");

    checkForAction(h, actions, "caret-backward");
    checkForAction(h, actions, "caret-begin-line");
    checkForAction(h, actions, "caret-end-line");
    checkForAction(h, actions, "caret-forward");
    checkForAction(h, actions, "caret-next-word");
    checkForAction(h, actions, "caret-previous-word");
    checkForAction(h, actions, "copy-to-clipboard");
    checkForAction(h, actions, "cut-to-clipboard");
    checkForAction(h, actions, "delete-next");
    checkForAction(h, actions, "delete-previous");
    checkForAction(h, actions, "paste-from-clipboard");
    checkForAction(h, actions, "select-all");
    checkForAction(h, actions, "selection-backward");
    checkForAction(h, actions, "selection-begin");
    checkForAction(h, actions, "selection-begin-line");
    checkForAction(h, actions, "selection-begin-word");
    checkForAction(h, actions, "selection-end");
    checkForAction(h, actions, "selection-end-line");
    checkForAction(h, actions, "selection-end-word");
    checkForAction(h, actions, "selection-forward");
    checkForAction(h, actions, "selection-next-word");
    checkForAction(h, actions, "selection-previous-word");
    checkForAction(h, actions, "toggle-componentOrientation");
    checkForAction(h, actions, "unselect");
    checkForAction(h, actions, "beep");
    checkForAction(h, actions, "caret-begin");
    checkForAction(h, actions, "caret-begin-paragraph");
    checkForAction(h, actions, "caret-begin-word");
    checkForAction(h, actions, "caret-down");
    checkForAction(h, actions, "caret-end");
    checkForAction(h, actions, "caret-end-paragraph");
    checkForAction(h, actions, "caret-end-word");
    checkForAction(h, actions, "caret-up");
    checkForAction(h, actions, "default-typed");
    checkForAction(h, actions, "dump-model");
    checkForAction(h, actions, "insert-break");
    checkForAction(h, actions, "insert-content");
    checkForAction(h, actions, "insert-tab");
    checkForAction(h, actions, "page-down");
    checkForAction(h, actions, "page-up");
    checkForAction(h, actions, "selection-begin-paragraph");
    checkForAction(h, actions, "selection-down");
    checkForAction(h, actions, "selection-end-paragraph");
    checkForAction(h, actions, "selection-page-down");
    checkForAction(h, actions, "selection-page-left");
    checkForAction(h, actions, "selection-page-right");
    checkForAction(h, actions, "selection-page-up");
    checkForAction(h, actions, "selection-up");
    checkForAction(h, actions, "select-line");
    checkForAction(h, actions, "select-paragraph");
    checkForAction(h, actions, "select-word");
    checkForAction(h, actions, "set-read-only");
    checkForAction(h, actions, "set-writable");
  }
}
