// Tags: JDK1.2

// Uses: ../AbstractButton/getActionCommand

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

package gnu.testlet.javax.swing.JToggleButton;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import javax.swing.JToggleButton;

/** <p>Tests the <code>JToggleButton</code>'s <code>getActionCommand</code>
 * method.</p>
 * 
 * <p>Please note that the interesting bits of this test reside in the superclass
 * {@link gnu.testlet.javax.swing.AbstractButton}
 * 
 * @author Robert Schuster
 *
 */
public class getActionCommand extends
		gnu.testlet.javax.swing.AbstractButton.getActionCommand implements
		Testlet {

	public void test(TestHarness harness) {
		check(new JToggleButton("bla"), harness);
	}

}
