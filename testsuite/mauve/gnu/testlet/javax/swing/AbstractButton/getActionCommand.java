//Tags: not-a-test

// Copyright (C) 2005 Robert Schuster <robertschuster@fsfe.org>

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

package gnu.testlet.javax.swing.AbstractButton;

import gnu.testlet.TestHarness;

import javax.swing.AbstractButton;

/**
 * Base class which implements the check to proof conformance of the
 * <code>getActionCommand</code> method of all <code>AbstractButton</code>
 * descendents.
 * 
 * @author Robert Schuster
 * 
 */
public class getActionCommand {

	protected void check(AbstractButton ab, TestHarness harness) {
		// The AbstractButton subclass instance should ...

		// ... use the label by default (test assumes it is set to "bla")
		harness.check(ab.getActionCommand(), "bla");

		ab.setText("foo");
		// ... change when the label changes
		harness.check(ab.getActionCommand(), "foo");

		ab.setText(null);
		// ... return null if the label is null
		harness.check(ab.getActionCommand(), null);

		ab.setActionCommand("baz");
		// return the ac as soon as it is set to a valid value
		harness.check(ab.getActionCommand(), "baz");

		ab.setText("bla");
		// and stay independent of the label changes
		harness.check(ab.getActionCommand(), "baz");

		ab.setText(null);
		// really
		harness.check(ab.getActionCommand(), "baz");

		ab.setActionCommand(null);
		ab.setText("GNU");
		// ... revert to default behavior when ac is unset
		harness.check(ab.getActionCommand(), "GNU");
	}
	
}
