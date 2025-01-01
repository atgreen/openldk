// Tags: GUI JDK1.0

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

package gnu.testlet.java.awt.FileDialog;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;

/**
 * Tests that FileDialog.getGraphics does not return null and that paint and
 * update are called on it.
 */
public class TestGraphics implements Testlet {
	FileDialog d = null;
	Robot r = null;

	boolean paintCalled = false;

	boolean updateCalled = false;

	public void test(TestHarness harness) {
		Frame f = new Frame();
		d = new FileDialog(f);
		r = harness.createRobot();
		
		f.setSize(200, 200);
		f.show();
		harness.check(d.getGraphics() == null);

		// A FileDialog is modal so we must cancel it so that the test will
		// finish. But a FileDialog's layout is peerset-dependant so we
		// cannot cancel it using Robot. We hide it from a separate
		// thread instead.
		new Thread() {
			public void run() {
				r.delay(2000);
				d.hide();
			}
		}.start();
		d.show();

		harness.check(d.getGraphics() != null);

		d.dispose();

		harness.check(d.getGraphics() == null);

		d = new FileDialog(f) {
			public void paint(Graphics g) {
				paintCalled = true;
			}

			public void update(Graphics g) {
				updateCalled = true;
			}
		};

		new Thread() {
			public void run() {
				r.delay(2000);
				d.hide();
			}
		}.start();
		d.show();

		harness.check(paintCalled);
		harness.check(!updateCalled);
		d.repaint();
		r.waitForIdle();
		r.delay(1000);

		// Sun does not call FileDialog.update.
		harness.check(!updateCalled);
	}
}
