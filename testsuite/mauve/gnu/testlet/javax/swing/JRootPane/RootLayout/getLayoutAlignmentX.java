// Tags: JDK1.2

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

package gnu.testlet.javax.swing.JRootPane.RootLayout;

import java.awt.LayoutManager2;

import javax.swing.JComponent;
import javax.swing.JRootPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getLayoutAlignmentX implements Testlet
{

  public void test(TestHarness harness)
  {
    JRootPane rp = new JRootPane();
    LayoutManager2 lm2 = (LayoutManager2) rp.getLayout();

    // Check for the value when nothing is touched.
    harness.check(lm2.getLayoutAlignmentX(rp), 0.0F);

    // Setting the root pane's alignmentX doesn't change anything.
    rp.setAlignmentX(0.5F);
    harness.check(lm2.getLayoutAlignmentX(rp), 0.0F);

    // Setting the content pane's alignmentX doesn't change anything.
    ((JComponent) rp.getContentPane()).setAlignmentX(0.5F);
    harness.check(lm2.getLayoutAlignmentX(rp), 0.0F);

    // Setting the glass pane's alignmentX doesn't change anything.
    ((JComponent) rp.getGlassPane()).setAlignmentX(0.5F);
    harness.check(lm2.getLayoutAlignmentX(rp), 0.0F);

    // Setting the layered pane's alignmentX doesn't change anything.
    ((JComponent) rp.getLayeredPane()).setAlignmentX(0.5F);
    harness.check(lm2.getLayoutAlignmentX(rp), 0.0F);
  }

}
