/* TreeRobot.java -- JTree test
 Copyright (C) 2006 Audrius Meskauskas
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

// Tags: JDK1.4 GUI


package gnu.testlet.javax.swing.JTree;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.Asserter;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.BitSet;
import java.util.Random;
import java.util.TreeSet;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

/**
 * The AWT robot based test for JTree.
 */
public class TreeRobot extends Asserter implements Testlet
{
  /**
   * The tree being tested.
   */
  JTree tree = new JTree();

  JFrame frame;

  Robot r;

  static Random ran = new Random();

  DefaultMutableTreeNode root = new DefaultMutableTreeNode("root");

  DefaultMutableTreeNode a = new DefaultMutableTreeNode("a");

  DefaultMutableTreeNode b = new DefaultMutableTreeNode("b");

  DefaultMutableTreeNode c = new DefaultMutableTreeNode("c");

  DefaultMutableTreeNode aa = new DefaultMutableTreeNode("aa");

  DefaultMutableTreeNode ab = new DefaultMutableTreeNode("ab");

  DefaultMutableTreeNode aaa = new DefaultMutableTreeNode("aaa");

  DefaultMutableTreeNode aab = new DefaultMutableTreeNode("aab");

  DefaultMutableTreeNode aac = new DefaultMutableTreeNode("aac");

  DefaultMutableTreeNode ba = new DefaultMutableTreeNode("ba");

  DefaultMutableTreeNode ca = new DefaultMutableTreeNode("ca");

  protected void setUp() throws Exception
  {

    frame = new JFrame();
    frame.getContentPane().add(tree);
    frame.setSize(400, 400);
    frame.setVisible(true);

    root.add(a);
    root.add(b);
    root.add(c);

    a.add(aa);
    a.add(ab);

    b.add(ba);

    c.add(ca);

    aa.add(aaa);
    aa.add(aab);
    aa.add(aac);

    DefaultTreeModel model = new DefaultTreeModel(root);
    tree.setModel(model);
    tree.setEditable(true);

    r = new Robot();
    r.setAutoDelay(50);
    r.delay(500);
  }

  protected void tearDown() throws Exception
  {
    frame.dispose();
  }

  /**
   * Click the mouse on center of the given component.
   */
  public void click(JComponent c, int x, int y)
  {
    Point p = new Point();
    p.x = x;
    p.y = y;

    SwingUtilities.convertPointToScreen(p, c);
    r.mouseMove(p.x, p.y);
    r.mousePress(InputEvent.BUTTON1_MASK);
    r.mouseRelease(InputEvent.BUTTON1_MASK);
  }

  /**
   * Click the given main databas view cell.
   * 
   * @param row
   *          the row
   * @param column
   *          the column
   */
  public void clickPath(JTree tree, TreePath path)
  {
    Rectangle rect = tree.getPathBounds(path);
    Point p = new Point(rect.x + rect.width / 2, rect.y + rect.height / 2);
    SwingUtilities.convertPointToScreen(p, tree);
    r.mouseMove(p.x, p.y);
    r.mousePress(InputEvent.BUTTON1_MASK);
    r.delay(50);
    r.mouseRelease(InputEvent.BUTTON1_MASK);
    r.delay(50);
  }

  public void typeDigit(char character)
  {
    int code = KeyEvent.VK_0 + (character - '0');
    r.keyPress(code);
    r.keyRelease(code);
  }

  public void type(int code)
  {
    r.keyPress(code);
    r.keyRelease(code);
  }
  
  public void testTree() throws Exception
  {
    // Test editing session on a root node, initiated with F2.
    assertEquals("Value before F2 editing", root.getUserObject(), "root");

    TreePath rootPath = tree.getPathForRow(0);
    clickPath(tree, rootPath);
    type(KeyEvent.VK_F2);
    type(KeyEvent.VK_END);
    
    typeDigit('0');
    type(KeyEvent.VK_ENTER);
    assertEquals("Value after F2 editing", root.getUserObject(),
                 "root0");
    
    // Test editing session, initiated with the click-pause- click.
    clickPath(tree, rootPath);
    r.delay(1000);    
    clickPath(tree, rootPath);    
    r.delay(1000);    
    type(KeyEvent.VK_END);
    typeDigit('1');
    
    type(KeyEvent.VK_ENTER);
    assertEquals("Value after click-pause-click", root.getUserObject(),
                 "root01");
    
    
    tree.setRootVisible(true);

    
    testNavigation();
  }

  public void testNavigation() throws Exception
  {
    // Test navigation with cursor keys:
    type(KeyEvent.VK_DOWN);
    assertEquals("navigation down 1", getFocus(), "a");
    
    type(KeyEvent.VK_DOWN);
    assertEquals("navigation down 2", getFocus(), "b");

    type(KeyEvent.VK_DOWN);
    assertEquals("navigation down 3", getFocus(), "c");
    
    type(KeyEvent.VK_UP);
    assertEquals("navigation up 1", getFocus(), "b");
    
    type(KeyEvent.VK_UP);
    assertEquals("navigation up 2", getFocus(), "a");
    
    // Expand the node a
    type(KeyEvent.VK_RIGHT);
    assertEquals("expansion", getFocus(), "a");    
    
    type(KeyEvent.VK_DOWN);
    assertEquals("navigation down after expansion 1", getFocus(), "aa");
    
    type(KeyEvent.VK_DOWN);
    assertEquals("navigation down after expansion 2", getFocus(), "ab");

    type(KeyEvent.VK_DOWN);
    assertEquals("navigation down after expansion 3", getFocus(), "b");
    
    type(KeyEvent.VK_UP);
    assertEquals("navigation up after expansion 1", getFocus(), "ab");

    type(KeyEvent.VK_UP);
    assertEquals("navigation up after expansion 2", getFocus(), "aa");
    
    type(KeyEvent.VK_RIGHT);
    assertEquals("expansion 2", getFocus(), "aa");    
    
    type(KeyEvent.VK_DOWN);
    assertEquals("navigation down after expansion 1", getFocus(), "aaa");
    
    type(KeyEvent.VK_DOWN);
    assertEquals("navigation down after expansion 2", getFocus(), "aab");

    type(KeyEvent.VK_DOWN);
    assertEquals("navigation down after expansion 3", getFocus(), "aac");
    
    type(KeyEvent.VK_DOWN);
    assertEquals("navigation up after expansion 1", getFocus(), "ab");

    type(KeyEvent.VK_DOWN);
    assertEquals("navigation up after expansion 2", getFocus(), "b");
    
    type(KeyEvent.VK_UP);
    type(KeyEvent.VK_UP);
    type(KeyEvent.VK_LEFT);
    assertEquals("navigation 1", getFocus(), "aa");    
    
    type(KeyEvent.VK_LEFT);
    assertEquals("navigation 1", getFocus(), "aa");    
    
    type(KeyEvent.VK_DOWN);
    assertEquals("navigation down after expansion 2", getFocus(), "ab");

    type(KeyEvent.VK_DOWN);
    assertEquals("navigation down after expansion 3", getFocus(), "b");
    
    // Try to expand the path with mouse
    TreePath bPath = tree.getSelectionPath();
    
    assertTrue("b path must be collapsed", !tree.isExpanded(bPath));
    Rectangle bounds = tree.getPathBounds(bPath);
    
    // It must be a metal tree icon, size about 18*18
    click(tree, bounds.x-9, bounds.y+9);
    r.delay(200);
    
    assertTrue("b path must be expanded", tree.isExpanded(bPath));
    
    // Navigate down, must be "ba":
    type(KeyEvent.VK_DOWN);
    assertEquals("navigation down after expansion 4", getFocus(), "ba");
    type(KeyEvent.VK_UP);
    
    // Click again to collapse:
    // It must be a metal tree icon, size about 18*18
    click(tree, bounds.x-9, bounds.y+9);
    r.delay(200);
    
    // Navigate down, must be "c":
    type(KeyEvent.VK_DOWN);
    assertEquals("navigation down after expansion 5", getFocus(), "c");
    
    // Cleanup for the subsequent round of the test:
    tree.setSelectionRow(0);
    
    // Collapse the currently expanded node a.
    type(KeyEvent.VK_LEFT);
  }
  
  public Object getFocus()
  {
    return ((DefaultMutableTreeNode)
        tree.getSelectionPath().getLastPathComponent()).getUserObject();
  }

  public void test(TestHarness harness)
  {
    try
      {
        h = harness;
        setUp();
        try
          {
            testTree();
          }
        finally
          {
            tearDown();
          }
      }
    catch (Exception e)
      {
        e.printStackTrace();
        h.fail("Exception: " + e);
      }
  }

}
