//Tags: JDK1.2

//Copyright (C) 2005 Red Hat

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

package gnu.testlet.javax.swing.JTextArea;

import java.awt.Robot;
import java.awt.AWTException;
import javax.swing.JFrame;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class isValidChecks implements Testlet 
{
  public void test(TestHarness harness)
  {
    JFrame jf = new JFrame();
    JTextArea area = new JTextArea(20, 45);
    JScrollPane scrollpane = new JScrollPane(area);
    Robot r = null;
    try
      {
        r = new Robot();
      }
    catch (AWTException e)
      {
        harness.fail("caught AWTException: "+e.getMessage());
      }

    jf.setContentPane(scrollpane);
    for (int i=0; i<80; i++) {
      area.append("line#" + i + "\n");
    };
    r.waitForIdle();
    jf.pack();
    r.waitForIdle();
    harness.checkPoint("append checks");
    area.append("");
    r.waitForIdle();
    harness.check(area.isValid(), true);
    area.append("lineNEw\n");
    r.waitForIdle();
    harness.check(area.isValid(), false);
    area.validate();
    
    harness.checkPoint("setRows checks");
    area.setRows(area.getRows());
    r.waitForIdle();
    harness.check(area.isValid(), true);
    area.setRows(area.getRows()+1);
    r.waitForIdle();
    harness.check(area.isValid(), false);
    area.validate();
    
    harness.checkPoint("setColumns checks");
    area.setColumns(area.getColumns());
    r.waitForIdle();
    harness.check(area.isValid(), true);
    area.setColumns(area.getColumns()+1);
    r.waitForIdle();
    harness.check(area.isValid(), false);
  }
}
