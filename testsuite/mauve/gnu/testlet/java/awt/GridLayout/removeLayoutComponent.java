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

package gnu.testlet.java.awt.GridLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.GridLayout;
import java.awt.Component;
import java.awt.Canvas;
import java.awt.Button;
import java.awt.Panel;

/**
 * Check for the functionality of method GridLayout.removeLayoutComponent
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
   * Test the method GridLayout.removeLayoutComponent for a Button widget.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  private void testButton(TestHarness harness) 
  {
    harness.checkPoint("removeLayoutComponent(Button)");
    GridLayout layout = new GridLayout();
    Button component = new Button();

    // Check the add/remove component method using all possible constraints.
    checkAllPossibleConstraints(harness, layout, component);
  }

  /**
   * Test the method GridLayout.removeLayoutComponent for a Canvas widget.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  private void testCanvas(TestHarness harness) 
  {
    harness.checkPoint("removeLayoutComponent(Canvas)");
    GridLayout layout = new GridLayout();
    Canvas component = new Canvas();

    // Check the add/remove component method using all possible constraints.
    checkAllPossibleConstraints(harness, layout, component);
  }

  /**
   * Check the add/remove component method using all possible constraints.
   * 
   * @param harness     the test harness (<code>null</code> not permitted).
   * @param layout      instance of GridLayout manager
   * @param component   selected component
   */
  private void checkAllPossibleConstraints(TestHarness harness, GridLayout layout, Component component)
  {
    checkAddRemove(harness, layout, component, "");
    checkAddRemove(harness, layout, component, " ");
    checkAddRemove(harness, layout, component, "xyzzy");
    checkAddRemove(harness, layout, component, null);
  }

  /**
   * Add specified component to the layout manager and then remove this component.
   * 
   * @param harness     the test harness (<code>null</code> not permitted).
   * @param layout      instance of GridLayout manager
   * @param component   selected component
   * @param name        component name
   */
  private void checkAddRemove(TestHarness harness, GridLayout layout, Component component, String name)
  {
    try {
      layout.addLayoutComponent(name, component);
    }
    catch (IllegalArgumentException e) {
      harness.fail(e.getMessage());
    }
    layout.removeLayoutComponent(component);
  }

}

