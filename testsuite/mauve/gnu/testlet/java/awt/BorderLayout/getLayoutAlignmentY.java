// Tags: JDK1.1

//Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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

package gnu.testlet.java.awt.BorderLayout;

import java.awt.BorderLayout;

import javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getLayoutAlignmentY implements Testlet
{

  @SuppressWarnings("serial")
  public void test(TestHarness harness)
  {
    // We use a JComponent here so we can modify the alignment property.
    JComponent rp = new JComponent(){
        // empty block is expected here
    };
    BorderLayout lm2 = new BorderLayout();

    // Check for the value when nothing is touched.
    harness.check(lm2.getLayoutAlignmentY(rp), 0.5F);

    // Setting the containers alignmentY doesn't change anything.
    rp.setAlignmentY(0.2F);
    harness.check(lm2.getLayoutAlignmentY(rp), 0.5F);
  }

}
