// Tags: JDK1.2 GUI
// Uses: ../../../java/awt/LocationTests

// Copyright (C) 2006 Red Hat

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

package gnu.testlet.javax.swing.JFrame;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.java.awt.LocationTests;

import javax.swing.JFrame;
import javax.swing.JComponent;
import java.awt.*;
import java.util.Vector;

/**
 * These tests pass with the Sun JDK 1.4.2_05 on GNU/Linux IA-32.
 */
public class HeavyweightComponent implements Testlet {

	// Tests that a heavyweight component is drawn at the correct location
	// within a JFrame.
	public void test(TestHarness harness) {
		int outer_width = 100;
		int outer_height = 100;
		int inner_width = 0;
		int inner_height = 0;

		Robot r = harness.createRobot();

		JFrame f = new JFrame();
		f.setLayout(null);
		Canvas c = new Canvas() {
			public void paint(Graphics g) {
				Rectangle r = getBounds();
				g.setColor(Color.red);
				// Subtract 1 from width and height since Graphics methods take
				// *co-ordinates* as arguments, not widths and heights.
				g.drawRect(r.x, r.y, r.width - 1, r.height - 1);
			}
		};
		f.setSize(outer_width, outer_height);
		f.add(c);
		f.show();

		// Wait for native frame to be fully displayed.
		r.waitForIdle();
		r.delay(1000);

		Insets i = f.getInsets();
		// Calculate area inside window frame.
		inner_width = outer_width - i.left - i.right;
		inner_height = outer_height - i.top - i.bottom;

		c.setBounds(0, 0, inner_width, inner_height);

		// bounds of red rectangle (1 pixel wide border)
		Point loc = f.getLocationOnScreen();
		Rectangle bounds = new Rectangle(loc.x + i.left, loc.y + i.top,
				inner_width, inner_height);

		// Wait for Canvas to paint itself.
		r.waitForIdle();
		r.delay(1000);

		// Check that the Canvas fills the entire JFrame, within the JFrame's
		// window frame.
		LocationTests.checkRectangleCornerColors(harness, r, bounds, Color.red,
				true);

		// There is a delay so the tester can see the result.
		r.delay(3000);
	}
}
