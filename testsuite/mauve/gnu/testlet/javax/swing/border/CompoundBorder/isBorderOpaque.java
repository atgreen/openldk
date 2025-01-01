/* isBorderOpaque.java
   Copyright (C) 2006 Red Hat
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.border.CompoundBorder;

import java.awt.Color;

import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class isBorderOpaque
    implements Testlet
{

  public void test(TestHarness harness)
  {
    CompoundBorder border = new CompoundBorder();
    harness.check(border.isBorderOpaque(), true);
    
    border = new CompoundBorder(null, null);
    harness.check(border.isBorderOpaque(), true);
    
    border = new CompoundBorder(new TitledBorder(""), new TitledBorder(""));
    harness.check(border.isBorderOpaque(), false);
    
    border = new CompoundBorder(null, new TitledBorder(""));
    harness.check(border.isBorderOpaque(), false);
    
    border = new CompoundBorder(new TitledBorder(""), null);
    harness.check(border.isBorderOpaque(), false);
    
    border = new CompoundBorder(new LineBorder(Color.red, 33, false), null);
    harness.check(border.isBorderOpaque(), true);
    harness.check(border.getInsideBorder() == null);
    harness.check(border.getOutsideBorder().isBorderOpaque(), true);
    
    border = new CompoundBorder(null, new LineBorder(Color.red, 33, false));
    harness.check(border.isBorderOpaque(), true);
    harness.check(border.getInsideBorder().isBorderOpaque(), true);
    harness.check(border.getOutsideBorder() == null);
    
    border = new CompoundBorder(new LineBorder(Color.red, 33, false), 
                                new LineBorder(Color.red, 33, false));
    harness.check(border.isBorderOpaque(), true);
    harness.check(border.getInsideBorder().isBorderOpaque(), true);
    harness.check(border.getOutsideBorder().isBorderOpaque(), true);
    
    border = new CompoundBorder(new LineBorder(Color.red, 33, true), null);
    harness.check(border.isBorderOpaque(), false);
    harness.check(border.getInsideBorder() == null);
    harness.check(border.getOutsideBorder().isBorderOpaque(), false);
    
    border = new CompoundBorder(null, new LineBorder(Color.red, 33, true));
    harness.check(border.isBorderOpaque(), false);
    harness.check(border.getInsideBorder().isBorderOpaque(), false);
    harness.check(border.getOutsideBorder() == null);
    
    border = new CompoundBorder(new LineBorder(Color.red, 33, true), 
                                new LineBorder(Color.red, 33, true));
    harness.check(border.isBorderOpaque(), false);
    harness.check(border.getInsideBorder().isBorderOpaque(), false);
    harness.check(border.getOutsideBorder().isBorderOpaque(), false);
    
  }
  
}
