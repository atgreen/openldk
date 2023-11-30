// Tags: JDK1.4

// Copyright (C) 2011 Pavel Tisnovsky <ptisnovs@redhat.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street,
// Fifth Floor, Boston, MA 02110-1301 USA.

package gnu.testlet.java.awt.BorderLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Canvas;
import java.awt.Button;
import java.awt.Panel;



/**
 * Check for the functionality of method BorderLayout.removeLayoutComponent
 */
public class removeLayoutComponent implements Testlet 
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    testButton(harness);
    testCanvas(harness);
  }

  /**
   * Test the method BorderLayout.removeLayoutComponent for a Button widget.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  private void testButton(TestHarness harness) 
  {
    harness.checkPoint("removeLayoutComponent(Button)");
    BorderLayout layout = new BorderLayout();
    Button component = new Button();

    // Check the add/remove component method using all possible constraints.
    checkAllPossibleConstraints(harness, layout, component);
  }

  /**
   * Test the method BorderLayout.removeLayoutComponent for a Canvas widget.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  private void testCanvas(TestHarness harness) 
  {
    harness.checkPoint("removeLayoutComponent(Canvas)");
    BorderLayout layout = new BorderLayout();
    Canvas component = new Canvas();

    // Check the add/remove component method using all possible constraints.
    checkAllPossibleConstraints(harness, layout, component);
  }

  /**
   * Check the add/remove component method using all possible constraints.
   * 
   * @param harness     the test harness (<code>null</code> not permitted).
   * @param layout      instance of BorderLayout manager
   * @param component   selected component
   */
  private void checkAllPossibleConstraints(TestHarness harness, BorderLayout layout, Component component)
  {
    checkAddRemove(harness, layout, component, BorderLayout.NORTH);
    checkAddRemove(harness, layout, component, BorderLayout.WEST);
    checkAddRemove(harness, layout, component, BorderLayout.SOUTH);
    checkAddRemove(harness, layout, component, BorderLayout.EAST);
    checkAddRemove(harness, layout, component, BorderLayout.PAGE_END);
    checkAddRemove(harness, layout, component, BorderLayout.PAGE_START);
    checkAddRemove(harness, layout, component, BorderLayout.LINE_END);
    checkAddRemove(harness, layout, component, BorderLayout.LINE_START);
    checkAddRemove(harness, layout, component, BorderLayout.AFTER_LAST_LINE);
    checkAddRemove(harness, layout, component, BorderLayout.AFTER_LINE_ENDS);
    checkAddRemove(harness, layout, component, BorderLayout.BEFORE_FIRST_LINE);
    checkAddRemove(harness, layout, component, BorderLayout.BEFORE_LINE_BEGINS);
  }

  /**
   * Add specified component to the layout manager and then remove this component.
   * 
   * @param harness     the test harness (<code>null</code> not permitted).
   * @param layout      instance of BorderLayout manager
   * @param component   selected component
   * @param constraints layout constraint
   */
  private void checkAddRemove(TestHarness harness, BorderLayout layout, Component component, String constraints)
  {
    try {
      layout.addLayoutComponent(component, constraints);
    }
    catch (IllegalArgumentException e) {
      harness.fail(e.getMessage());
    }
    layout.removeLayoutComponent(component);
  }

}

