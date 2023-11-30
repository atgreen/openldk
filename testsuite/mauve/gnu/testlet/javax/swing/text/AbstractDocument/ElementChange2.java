// Tags: JDK1.2

// Copyright (C) 2005 Red Hat.

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

package gnu.testlet.javax.swing.text.AbstractDocument;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.text.*;
import javax.swing.*;
import javax.swing.event.*;

/**
 * Tests if adding and removing content fires DocumentEvents with the proper ElementChange.
 */
public class ElementChange2
  implements Testlet
{  
  public void test(TestHarness harness)
  {
    JTextArea textArea = new JTextArea ();
    final TestHarness harness2 = harness;
    textArea.setText("0123456");
    ((AbstractDocument)textArea.getDocument()).addDocumentListener(new DocumentListener(){
        public void changedUpdate (DocumentEvent e)
        {
        }

        public void insertUpdate (DocumentEvent e)
        {
          Element root = e.getDocument().getDefaultRootElement();
          DocumentEvent.ElementChange ec = e.getChange(root);                
          harness2.checkPoint ("insertUpdate without adding children");
          if (ec != null)
            harness2.fail("Element Change should be null");
        }

        public void removeUpdate (DocumentEvent e)
        {
          Element root = e.getDocument().getDefaultRootElement();
          DocumentEvent.ElementChange ec = e.getChange(root);                
          harness2.checkPoint ("removeUpdate without removing children");
          if (ec != null)
            harness2.fail("ElementChange should be null");
        }
      });

    textArea.append("7");
    try
      {
        textArea.getDocument().remove(3, 1);
      }
    catch (BadLocationException ble)
      {
      }

  }
}
