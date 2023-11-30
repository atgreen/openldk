// Tags: JDK1.2

// Copyright (C) 2004, 2005 Arnaud Vandyck <avdyk@gnu.org>

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

package gnu.testlet.javax.swing.text.StringContent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.BadLocationException;
import javax.swing.text.StringContent;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoableEdit;

public class removeUndo implements Testlet
{
  public void test(TestHarness h)
  {
    h.checkPoint("StringContent");

    StringContent sc = new StringContent();
    UndoableEdit ue = null;
    UndoableEdit ue2 = null;
    UndoableEdit ue3 = null;
    try
      {
        h.checkPoint("StringContent -- insertString()");
        sc.insertString(0, "path");
        h.check("path\n", sc.getString(0, sc.length()),
                "StringContent.insertString(): insert 'path' at 0");
        sc.insertString(0, "class");
        ue = sc.remove(1, 4);
        java.util.Locale.setDefault(java.util.Locale.US);
        String presentationName = ue.getPresentationName();
        h.check("", presentationName, "PresentationName should be '' and is: " + presentationName);
        String redoPresentationName = ue.getRedoPresentationName();
        h.check("Redo", redoPresentationName, "RedoPresentationName should be Redo and is: " + redoPresentationName);
        String undoPresentationName = ue.getUndoPresentationName();
        h.check("Undo", undoPresentationName, "UndoPresentationName should be Undo and is: " + undoPresentationName);
        h.check(false, ue.canRedo(), "canRedo? () (" + ue.canRedo() + ")");
        h.check(true, ue.canUndo(), "canUndo? () (" + ue.canUndo() + ")");
        h.check("cpath\n", sc.getString(0, sc.length()),
                "Remove path: getString should be class\\n and is: " + sc.getString(0, sc.length()));
        ue.undo();
        h.check("classpath\n", sc.getString(0, sc.length()),
                "Undo: getString should be classpath\\n and is: " + sc.getString(0, sc.length()));
        h.check(true, ue.canRedo(), "canRedo? () (" + ue.canRedo() + ")");
        h.check(false, ue.canUndo(), "canUndo? () (" + ue.canUndo() + ")");
        ue.redo();
        h.check("cpath\n", sc.getString(0, sc.length()),
                "Redo: getString should be cpath\\n and is : " + sc.getString(0, sc.length()));
        ue.die();
        h.debug("UndoableEdit.die() no more undo/redo");
        h.check(false, ue.canUndo(), "die, no more undo");
        h.check(false, ue.canRedo(), "die, no more redo");
        sc = new StringContent();
        sc.insertString(0, "classpathX");
        h.check("classpathX\n", sc.getString(0, sc.length()), "should be 'classpathX' and is: " + sc.getString(0, sc.length()));
        ue = sc.remove(3, 2);
        h.check("clapathX\n", sc.getString(0, sc.length()),
                "double undo: should be 'clapathX\\n' and is: " + sc.getString(0, sc.length()));
        ue2 = sc.remove(4, 2);
        h.check("claphX\n", sc.getString(0, sc.length()),
                "double undo: should be 'claphX\\n' and is: " + sc.getString(0, sc.length()));
        ue3 = sc.remove(0, 3);
        h.check(true, ue.canUndo(), "double undo can undo?");
        h.check("phX\n", sc.getString(0, sc.length()), "check remove: should be 'phX\\n' and is: " + sc.getString(0, sc.length()));
        ue.undo();
        h.check("phXss\n", sc.getString(0, sc.length()),
                "double undo: should be 'phXss\\n' and is: " + sc.getString(0, sc.length()));
        ue2.undo();
        h.check("phXsats\n", sc.getString(0, sc.length()), "should be 'phXsats\\n' and is: " + sc.getString(0, sc.length()));
        ue.redo();
        h.check("phXts\n", sc.getString(0, sc.length()), "double undo: should be 'phXts\\n' and is: " + sc.getString(0, sc.length()));
        ue3.undo();
        h.check("claphXts\n", sc.getString(0, sc.length()), "add an X: should be 'claphXts\\n' and is: " + sc.getString(0, sc.length()));
      }
    catch (BadLocationException ble)
      {
        h.fail("BadLocation! " + ble.getMessage());
      }
    try{
      ue3.undo();
      h.fail("should not be able to undo!");
    }
    catch (CannotUndoException cannot)
      {
        h.checkPoint("cannot undo");
      }
    try
      {
        sc = new StringContent();
        sc.insertString(0, "super classpath");
        h.check("super classpath\n", sc.getString(0, sc.length()), "insert 'super classpath': " + sc.getString(0, sc.length()));
        ue = sc.remove(0, 6);
        h.check("classpath\n", sc.getString(0, sc.length()), "insert 'super classpath': " + sc.getString(0, sc.length()));
        ue.undo();
        h.check("super classpath\n", sc.getString(0, sc.length()), "undo 'classpath': " + sc.getString(0, sc.length()));
        ue.undo();
        h.fail("should not be able to undo two times");
      }
    catch (BadLocationException ble)
      {
        h.fail("BadLocation! " + ble.getMessage());
      }
    catch (CannotUndoException cannot)
      {
        h.checkPoint("cannot undo several times");
      }
  }

}

