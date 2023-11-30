/* Test15.java -- Test 1.5 additions to BorderLayout
   Copyright (C) 2005 Red Hat, Inc.
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

// Tags: JDK1.5

package gnu.testlet.java.awt.BorderLayout;

import java.awt.BorderLayout;
import java.awt.Container;

import javax.swing.JPanel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class Test15 implements Testlet {

	@SuppressWarnings("deprecation")
    public void test(TestHarness harness) {
		BorderLayout layout = new BorderLayout();
		JPanel center = new JPanel();
		JPanel left = new JPanel();
		JPanel first = new JPanel();
		JPanel notInLayout = new JPanel();
		Container container = new Container();

		layout.addLayoutComponent(BorderLayout.CENTER, center);
		layout.addLayoutComponent(BorderLayout.WEST, left);
		layout.addLayoutComponent(BorderLayout.LINE_START, first);

		harness.checkPoint("getConstraints");
		harness.check(layout.getConstraints(center), BorderLayout.CENTER);
		harness.check(layout.getConstraints(left), BorderLayout.WEST);
		harness.check(layout.getConstraints(first), BorderLayout.LINE_START);
		harness.check(layout.getConstraints(null), null);
		harness.check(layout.getConstraints(notInLayout), null);
		
		harness.checkPoint("getLayoutComponent");
		harness.check(layout.getLayoutComponent(BorderLayout.CENTER), center);
		harness.check(layout.getLayoutComponent(BorderLayout.AFTER_LAST_LINE),
				null);
		harness.check(layout.getLayoutComponent(BorderLayout.WEST), left);
		boolean ok = false;
		try
		{
			layout.getLayoutComponent("HiMaude");
		}
		catch (IllegalArgumentException _)
		{
			ok = true;
		}
		harness.check(ok);
		
		harness.checkPoint("getLayoutComponent with Container");
		harness.check(layout.getLayoutComponent(container, BorderLayout.CENTER),
				center);
		harness.check(layout.getLayoutComponent(container, 
				BorderLayout.EAST), null);
		harness.check(layout.getLayoutComponent(container, 
				BorderLayout.WEST), first);
		ok = false;
		try
		{
			layout.getLayoutComponent(container, BorderLayout.AFTER_LAST_LINE);
		}
		catch (IllegalArgumentException _)
		{
			ok = true;
		}
		harness.check(ok);
		
		ok = false;
		try
		{
			layout.getLayoutComponent(container, "HiMaude");
		}
		catch (IllegalArgumentException _)
		{
			ok = true;
		}
		harness.check(ok);
		
		ok = false;
		try
		{
			layout.getLayoutComponent(null, BorderLayout.WEST);
		}
		catch (NullPointerException _)
		{
			ok = true;
		}
		harness.check(ok);
	}
}
