// Tags: JDK1.5 

// Copyright (C) 2005, 2006, David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.javax.swing.plaf.metal.DefaultMetalTheme;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Font;

import javax.swing.UIManager;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.metal.DefaultMetalTheme;

/**
 * Some checks for the getSystemTextFont() method.
 */
public class getSystemTextFont implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    DefaultMetalTheme t = new DefaultMetalTheme();
    FontUIResource f = t.getSystemTextFont();
    harness.check(f, new FontUIResource("Dialog", Font.PLAIN, 12));
    FontUIResource f2 = t.getSystemTextFont();
    harness.check(f == f2);
    
    // setting defaults property doesn't affect this font...
    UIManager.put("swing.boldMetal", Boolean.TRUE);
    f = t.getSystemTextFont();
    harness.check(f, new FontUIResource("Dialog", Font.PLAIN, 12));
    // ...but is picked up by new themes
    DefaultMetalTheme t2 = new DefaultMetalTheme();
    f = t2.getSystemTextFont();
    harness.check(f, new FontUIResource("Dialog", Font.PLAIN, 12)); 
    
    // set it to false
    UIManager.put("swing.boldMetal", Boolean.FALSE);
    DefaultMetalTheme t3 = new DefaultMetalTheme();
    f = t3.getSystemTextFont();
    harness.check(f, new FontUIResource("Dialog", Font.PLAIN, 12)); 

    // clear it again
    UIManager.put("swing.boldMetal", null);
    DefaultMetalTheme t4 = new DefaultMetalTheme();
    f = t4.getSystemTextFont();
    harness.check(f, new FontUIResource("Dialog", Font.PLAIN, 12)); 
  }

}
