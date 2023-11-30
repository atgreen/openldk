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

import javax.swing.JTextArea;
import javax.swing.text.BadLocationException;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Test if the carret mark is always less or equal to the carret dot.
 * Should not require GUI to run.
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class gettingText
  implements Testlet
{
  public void test(TestHarness harness)
  {
    String content = "abcdefghijklmnopqrstuvwxyz";
    JTextArea area = new JTextArea(content);

    int l = content.length();

    int mark;
    int dot;

    test:
    for (int a = 0; a < l; a++)
      for (int b = 0; b < l; b++)
        {
          area.setSelectionStart(a);
          area.setSelectionEnd(b);

          mark = area.getCaret().getMark();
          dot = area.getCaret().getDot();

          if (mark > dot)
            {
              harness.fail("mark, " + mark + " > dot, " + dot +
                           " when setting [" + a + ".." + b + "]"
                          );
              break test;
            }
        }

    String s;
    testSt:
    for (int a = 0; a < l; a++)
      for (int b = a + 1; b < l; b++)
        {
          s = content.substring(a, b);
          try
            {
              area.setSelectionStart(a);
              area.setSelectionEnd(b);

              if (!area.getSelectedText().equals(s))
                {
                  harness.check(area.getText(a, b), s,
                                "getSelectedText [" + a + "," + b + "]"
                               );
                  break testSt;
                }

              area.select(1,2);
              area.select(a, b);

              if (!area.getSelectedText().equals(s))
                {
                  harness.check(area.getSelectedText(), s,
                                "getSelectedText select[" + a + "," + b + "]"
                               );
                  break testSt;
                }


               if (!area.getText(a, b-a).equals(s))
               {
                 harness.check(area.getText(a, b-a), s,
                 "getText [" + a + "," + (b-a) + "]");
               }
            }
          catch (BadLocationException ex)
            {
              harness.fail("BadLocationException in getSelectedText [" + a +
                           "," + b + "]"
                          );
            }
        }

        // test moveCaretPosition
        area.setSelectionStart(5);
        area.moveCaretPosition(0);
        harness.check(area.getSelectedText(), "abcde", "moveCaretPositio()");
  }
}
