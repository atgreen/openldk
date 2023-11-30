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
public class ElementChange
  implements Testlet
{  
  public void test(TestHarness harness)
  {
    JTextArea textArea = new JTextArea ();
    final TestHarness harness2 = harness;

    ((AbstractDocument)textArea.getDocument()).addDocumentListener(new DocumentListener(){
        public void changedUpdate (DocumentEvent e)
        {
        }

        public void insertUpdate (DocumentEvent e)
        {
          Element root = e.getDocument().getDefaultRootElement();
          DocumentEvent.ElementChange ec = e.getChange(root);                
          Element[] childrenAdded = ec.getChildrenAdded();
          Element[] childrenRemoved = ec.getChildrenRemoved();
          harness2.checkPoint("insert update children added");
          harness2.check(childrenAdded.length == 4);
          harness2.check(childrenAdded[0].getStartOffset() == 0);
          harness2.check(childrenAdded[0].getEndOffset() == 36);
          harness2.check(childrenAdded[1].getStartOffset() == 36);
          harness2.check(childrenAdded[1].getEndOffset() == 97);
          harness2.check(childrenAdded[2].getStartOffset() == 97);
          harness2.check(childrenAdded[2].getEndOffset() == 134);
          harness2.check(childrenAdded[3].getStartOffset() == 134);
          harness2.check(childrenAdded[3].getEndOffset() == 176);

          harness2.checkPoint("insert update children removed");
          harness2.check(childrenRemoved.length == 1);
          harness2.check(childrenRemoved[0].getStartOffset() == 0);
          harness2.check(childrenRemoved[0].getEndOffset() == 176);
        }

        public void removeUpdate (DocumentEvent e)
        {
          Element root = e.getDocument().getDefaultRootElement();
          DocumentEvent.ElementChange ec = e.getChange(root);                
          Element[] childrenAdded = ec.getChildrenAdded();
          Element[] childrenRemoved = ec.getChildrenRemoved();
          harness2.checkPoint("remove update children added");
          harness2.check(childrenAdded.length == 1);
          harness2.check(childrenAdded[0].getStartOffset() == 0);
          harness2.check(childrenAdded[0].getEndOffset() == 57);

          harness2.checkPoint("remove udpate childrem removed");
          harness2.check(childrenRemoved.length == 2);
          harness2.check(childrenRemoved[0].getStartOffset() == 0);
          harness2.check(childrenRemoved[0].getEndOffset() == 5);
          harness2.check(childrenRemoved[1].getStartOffset() == 5);
          harness2.check(childrenRemoved[1].getEndOffset() == 57);
        }
      });
    textArea.setText("This is the text that we are adding\nIt has several lines, which should be several children added\nWhile only one child will be removed\nThat is, if the implementation is correct");
    try
      {
        textArea.getDocument().remove(5, 40);
      }
    catch (BadLocationException ble)
      {
      }

  }
}
