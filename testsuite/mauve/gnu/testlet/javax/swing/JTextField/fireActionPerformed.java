/* fireActionPerfomed.java 
   Copyright (C) 2006 
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

// Tags: 1.4

package gnu.testlet.javax.swing.JTextField;

import java.awt.Point;
import java.awt.Robot;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import javax.swing.JFrame;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class fireActionPerformed implements Testlet
{

  JFrame frame;
  JTextField text;
  Robot robot;
  
  protected void setUp1(final TestHarness harness) throws Exception
  {    
    text = new JTextField();
    text.setActionCommand(null);
    text.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event)
      {
        harness.check(event.getActionCommand() != null);
        harness.check(event.getActionCommand(), text.getText());
      }
    });
    
    frame = new JFrame();
    frame.setSize(200, 200);
    frame.getContentPane().add(text);
    frame.show();
    
    robot = new Robot();
    robot.setAutoDelay(50);
    robot.delay(500);
  }
  
  protected void setUp2(final TestHarness harness) throws Exception
  {    
    text = new JTextField();
    text.setActionCommand("Action Command");
    text.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event)
      {
        harness.check(event.getActionCommand() != null);
        harness.check(event.getActionCommand() != text.getText());
        harness.check(event.getActionCommand() == "Action Command");
      }
    });
    
    frame = new JFrame();
    frame.setSize(200, 200);
    frame.getContentPane().add(text);
    frame.show();
    
    robot = new Robot();
    robot.setAutoDelay(50);
    robot.delay(500);
  }
  
  protected void tearDown() throws Exception
  {
    frame.dispose();
  }
  
  public void click(JTextField text, int x, int y)
  {
    Point p = new Point();
    p.x = x;
    p.y = y;

    SwingUtilities.convertPointToScreen(p, text);
    robot.mouseMove(p.x, p.y);
    robot.mousePress(InputEvent.BUTTON1_MASK);
    robot.mouseRelease(InputEvent.BUTTON1_MASK);
  }
  
  public void type(int key)
  {
    robot.keyPress(key);
    robot.keyRelease(key);
  }
  
  public void enterInput(TestHarness harness)
  {   
    type(KeyEvent.VK_U);
    type(KeyEvent.VK_S);
    type(KeyEvent.VK_E);
    type(KeyEvent.VK_R);
    type(KeyEvent.VK_SPACE);
    type(KeyEvent.VK_I);
    type(KeyEvent.VK_N);
    type(KeyEvent.VK_P);
    type(KeyEvent.VK_U);
    type(KeyEvent.VK_T);
    type(KeyEvent.VK_ENTER);
  }
  
  public void test(TestHarness harness) 
  {
    // This test ensures that if actionCommand == null,
    // then the user's input is used as the actionCommand.
    try
      {
        setUp1(harness);
        try 
          {
            click(text, text.getWidth() / 2, text.getHeight() / 2);
            enterInput(harness);
          }
        finally
          {
            tearDown();
          }
      }
    catch (Exception e)
      {
        e.printStackTrace();
        harness.fail("Exception: " + e);
      }
    
    // This test ensures that if actionCommand != null,
    // then the actionCommand is used.
    try
    {
      setUp2(harness);
      try 
        {
          click(text, text.getWidth() / 2, text.getHeight() / 2);
          enterInput(harness);
        }
      finally
        {
          tearDown();
        }
    }
  catch (Exception e)
    {
      e.printStackTrace();
      harness.fail("Exception: " + e);
    }
  }
 
}
