// Tags: GUI JDK1.2

// Copyright (C) 2005, 2006, Red Hat

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
// along with Mauve; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301 USA.

package gnu.testlet.java.awt.Component;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.dnd.DropTarget;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Locale;

/**
 * Check if bound properties are firing PropertyChangeEvents and
 * simple properties not.
 *
 * @author Roman Kennke (roman@ontographics.com)
 */
public class properties implements Testlet
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


    // check 'background' property (must be fired)
    propertyName = null;
    comp.setBackground(Color.YELLOW);
    harness.check(propertyName, "background", "Property: background");

    // check 'bounds' property (must not be fired)
    propertyName = null;
    comp.setBounds(new Rectangle(143, 564, 1200, 2233));
    harness.check(propertyName, null, "Property: bounds");

    // check 'componentOrientation' property (should be fired)
    propertyName = null;
    comp.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
    // do second call to assure that the property actually changes
    comp.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
    harness.check(propertyName, "componentOrientation",
                  "Property: componentOrientation");

    // check 'cursor' property (must not be fired)
    propertyName = null;
    comp.setCursor(new Cursor(Cursor.HAND_CURSOR));
    harness.check(propertyName, null, "Property: cursor");

    // check 'dropTarget' property (must not be fired)
    propertyName = null;
    comp.setDropTarget(new DropTarget());
    harness.check(propertyName, null, "Property: dropTarget");

    // check 'enabled' property (must not be fired)
    propertyName = null;
    comp.setEnabled(true);
    comp.setEnabled(false);
    harness.check(propertyName, null, "Property: enabled");

    // check 'font' property (must be fired)
    propertyName = null;
    comp.setFont(new Font("Monospaced", Font.PLAIN, 12));
    harness.check(propertyName, "font", "Property: font");

    // check 'foreground' property (must be fired)
    propertyName = null;
    comp.setForeground(Color.CYAN);
    harness.check(propertyName, "foreground", "Property: foreground");

    // check 'locale' property (must be fired)
    propertyName = null;
    comp.setLocale(Locale.CHINESE);
    comp.setLocale(Locale.GERMAN);
    harness.check(propertyName, "locale", "Property: locale");

    // check 'location' property (must not be fired)
    propertyName = null;
    comp.setLocation(new Point(123, 456));
    harness.check(propertyName, null, "Property: location");

    // check 'name' property (must be fired)
    propertyName = null;
    comp.setName("Obelix");
    harness.check(propertyName, "name", "Property: name");

    // check 'size' property (must not be fired)
    propertyName = null;
    comp.setSize(new Dimension(987, 654));
    harness.check(propertyName, null, "Property: size");

    // check 'visible' property (must not be fired)
    propertyName = null;
    comp.setVisible(true);
    comp.setVisible(false);
    harness.check(propertyName, null, "Property: visible");

  }
}
