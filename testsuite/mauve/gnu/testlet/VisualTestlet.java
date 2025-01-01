/* VisualTestlet.java -- Abstract superclass for visual tests
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
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

package gnu.testlet;

import java.awt.Component;
import java.awt.Frame;
import java.io.IOException;

import javax.swing.JComponent;
import javax.swing.JFrame;

/**
 * Provides an environment for visual tests. Visual tests must provide a
 * component, instructions and the expected results. The harness provides
 * all three to the tester and ask if the test passed or not.
 *
 * The test component is displayed inside a AWT Frame or a Swing JFrame
 * (depending on the type of the component). This means that the tested
 * Java environment needs to have some basic AWT or Swing functionality. This
 * should be covered by other tests (possibly by java.awt.Robot or so).
 */
public abstract class VisualTestlet
  implements Testlet
{

  /**
   * Starts the test.
   *
   * @param h the test harness
   */
  public void test(TestHarness h)
  {
    // Initialize and show test component.
    Component c = getTestComponent();
    Frame f;
    if (c instanceof JComponent)
      {
        JFrame jFrame = new JFrame();
        jFrame.setContentPane((JComponent) c);
        f = jFrame;
      }
    else
      {
        f = new Frame();
        f.add(c);
      }
    f.pack();
    f.setVisible(true);

    // Print instructions and expected results on console.
    System.out.println("====================================================");
    System.out.print("This is a test that needs human interaction. Please ");
    System.out.print("read the instructions carefully and follow them. ");
    System.out.print("Then check if your results match the expected results. ");
    System.out.print("Type p <ENTER> if the test showed the expected results,");
    System.out.println(" f <ENTER> otherwise.");
    System.out.println("====================================================");
    System.out.println("INSTRUCTIONS:");
    System.out.println(getInstructions());
    System.out.println("====================================================");
    System.out.println("EXPECTED RESULTS:");
    System.out.println(getExpectedResults());
    System.out.println("====================================================");

    // Ask the tester whether the test passes or fails.
    System.out.println("(P)ASS or (F)AIL ?");
    while (true)
      {
        int ch;
        try
          {
            ch = System.in.read();
            if (ch == 'P' || ch == 'p')
              {
                h.check(true);
                break;
              }
            else if (ch == 'f' || ch == 'F')
              {
                h.check(false);
                break;
              }
          }
        catch (IOException ex)
          {
            h.debug(ex);
            h.fail("Unexpected IO problem on console");
          }
      }
  }

  /**
   * Provides the instructions for the test.
   *
   * @return the instructions for the test
   */
  public abstract String getInstructions();

  /**
   * Describes the expected results.
   *
   * @return the expected results
   */
  public abstract String getExpectedResults();

  /**
   * Provides the test component.
   *
   * @return the test component
   */
  public abstract Component getTestComponent();
}
