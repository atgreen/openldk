// Tags: GUI JDK1.2

// Copyright (C) 2005 Red Hat

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

package gnu.testlet.java.awt.Component;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;

/**
 * The method invalidate should be automatically called in
 * response to several property changes such as size and location.
 * This is checked here.
 *
 * @author Roman Kennke (roman@ontographics.com)
 */
public class invalidate implements Testlet
{

  /**
   * Non abstract subclass of Component to allow instatiation and
   * test.
   */
  public class TestComponent extends Component
  {

    /**
     * If revalidate has been called or not.
     */
    boolean invalidated;

    /**
     *  Override this method to check if revalidate has been called.
     */
    public void invalidate()
    {
      invalidated = true;
      super.invalidate();
    }
  }

  /**
   * Subclass of Container to allow  test.
   */
  class TestContainer extends Container
  {
    boolean invalidated;

    /**
     *  Override this method to check if revalidate has been called.
     */
    public void invalidate()
    {
      invalidated = true;
      super.invalidate();
    }
  }
  
  /**
   * Subclass of Component to allow  test.
   */
  public class myComponent
      extends Component
  {
  }

  public void test (TestHarness harness)
  {
    test1(harness);
    test2(harness);
    testInvalidateInvalidComponent(harness);
  }

  private void test1(TestHarness harness)
  {
    // prepare test component
    TestComponent comp = new TestComponent();
    Frame frame = new Frame();
    frame.add(comp);
    frame.setVisible(true);

    // change size and check if invalidate has been called
    comp.invalidated = false;
    comp.setSize(100, 200);
    harness.check(comp.invalidated, true);

    // change size and check if invalidate has been called
    comp.invalidated = false;
    comp.setSize(new Dimension(101, 201));
    harness.check(comp.invalidated, true);

    // change size and check if invalidate has been called
    comp.invalidated = false;
    comp.resize(102, 202);
    harness.check(comp.invalidated, true);

    frame.dispose();
  }
  
  private void test2(TestHarness harness)
  {
    myComponent c = new myComponent();
    harness.check(c.isPreferredSizeSet(), false);
    
    c.setPreferredSize(new Dimension(400, 500));
    Dimension prefSizeOld = c.getPreferredSize();
    harness.check(c.isPreferredSizeSet(), true);
    
    c.invalidate();
    harness.check(c.isPreferredSizeSet(), true);
    Dimension prefSizeNew = c.getPreferredSize();
    harness.check(prefSizeOld != prefSizeNew);
    harness.check(prefSizeOld.equals(prefSizeNew));
  }

  private void testInvalidateInvalidComponent(TestHarness harness)
  {
    harness.checkPoint("invalidateInvalidComponent");
    Frame f = new Frame();
    TestContainer c1 = new TestContainer();
    TestComponent c2 = new TestComponent();
    c1.add(c2);
    f.add(c1);
    f.setSize(100, 100);
    f.setVisible(true);
    c1.validate();
    c2.validate();
    harness.check(c1.isValid(), true);
    harness.check(c1.isValid(), true);
    c1.invalidated = false;
    c2.invalidated = false;
    // This should invalidate both c1 and c2.
    c2.invalidate();
    harness.check(c1.invalidated, true);
    harness.check(c2.invalidated, true);

    // Now both components are invalid. Another call to invalidate() on c2
    // should not invalidate c1, since it's already invalid.
    c1.invalidated = false;
    c2.invalidated = false;
    // This should invalidate both c1 and c2.
    c2.invalidate();
    harness.check(c1.invalidated, false);
    harness.check(c2.invalidated, true);
    f.dispose();
  }

}
