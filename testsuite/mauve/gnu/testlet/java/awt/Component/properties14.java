// Tags: GUI JDK1.4

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
import java.beans.*;


/**
 * Check if bound properties are firing PropertyChangeEvents and
 * simple properties not.
 *
 * @author Roman Kennke (roman@ontographics.com)
 */
public class properties14 implements Testlet
{

  /**
   * stores the name of a fired property or <code>null</code> of none has
   * been fired.
   */
  private String propertyName;

  /**
   * Non abstract subclass of Component to allow instatiation.
   */
  public class TestComponent extends Component {
  }

  public void test (TestHarness harness)
  {
    // prepare test component
    Component comp = new TestComponent();
    comp.addPropertyChangeListener(new PropertyChangeListener() {
            // sets <code>propertyName</code> when called
            public void propertyChange(PropertyChangeEvent ev) {
                propertyName = ev.getPropertyName();
            }
        });


    // check 'focusable' property
    propertyName = null;
    comp.setFocusable(false);
    comp.setFocusable(true);
    harness.check(propertyName, "focusable", "Property: focusable");

    // check 'focusTraversalKeysEnabled' property
    propertyName = null;
    comp.setFocusTraversalKeysEnabled(false);
    comp.setFocusTraversalKeysEnabled(true);
    harness.check(propertyName, "focusTraversalKeysEnabled",
                  "Property: focusTraversalKeysEnabled");

    // check 'ignoreRepaint' property
    propertyName = null;
    comp.setIgnoreRepaint(false);
    comp.setIgnoreRepaint(true);
    harness.check(propertyName, null, "Property: ignoreRepaint");

  }
}
