//Tags: JDK1.2 GUI

//Copyright (C) 2005 Robert Schuster <thebohemian@gmx.net>

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.JDialog;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import javax.swing.JDialog;

/**
 * <p>This tests whether setDefaultCloseOperation() accepts all
 * kinds of values whether they are senseful or not.</p>
 * 
 * <p>This triggered GNU Classpath bug #12205.</p>
 * 
 * <p>TODO: One could make a graphical test for this. For invalid
 * values the behavior should be like DO_NOTHING_ON_CLOSE</p>
 */ 
public class setDefaultCloseOperation implements Testlet {
	
	public void test(TestHarness harness) {
		JDialog dialog = new JDialog();

		harness.checkPoint("default value");
		harness.check(dialog.getDefaultCloseOperation(), JDialog.HIDE_ON_CLOSE);

		harness.checkPoint("valid values");
		
		dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		harness.check(dialog.getDefaultCloseOperation(),
				JDialog.DISPOSE_ON_CLOSE);

		dialog.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
		harness.check(dialog.getDefaultCloseOperation(),
				JDialog.DO_NOTHING_ON_CLOSE);

		dialog.setDefaultCloseOperation(JDialog.HIDE_ON_CLOSE);
		harness.check(dialog.getDefaultCloseOperation(), JDialog.HIDE_ON_CLOSE);

		harness.checkPoint("invalid values");
		
		dialog.setDefaultCloseOperation(JDialog.EXIT_ON_CLOSE);
		harness.check(dialog.getDefaultCloseOperation(), JDialog.EXIT_ON_CLOSE);

		// meaningless number
		final int SOME_RANDOM = 434323423;
		
		dialog.setDefaultCloseOperation(SOME_RANDOM);
		harness.check(dialog.getDefaultCloseOperation(), SOME_RANDOM);
	}

}
