//Tags: JDK1.2

//Copyright (C) 2004 Sven de Marothy <sven@physto.se>

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
//Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.geom.Line2D;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.geom.Line2D;

/**
 * Checks whether Line2D.linesIntersect works correctly
 *
 * @author Sven de Marothy
 */
public class linesIntersect
  implements Testlet
{
  public void test(TestHarness harness)
  {
      // Test 1 - a simple intersection
      harness.check(Line2D.linesIntersect(0.0, 0.0, 100.0, 50.0,
					  0.0, 50.0, 100.0, 0.0));

      // Test 2 - an orthogonal intersection
      harness.check(Line2D.linesIntersect(0.0, 0.0, 100.0, 100.0,
					  0.0, 100.0, 100.0, 0.0));

      // Test 3 - an orthogonal intersection on the axes
      harness.check(Line2D.linesIntersect(0.0, 10.0, 100.0, 10.0,
					  50.0, 0.0, 50.0, 50.0));
   
      // Test 4 - colinear overlapping lines
      harness.check(Line2D.linesIntersect(10.0, 10.0, 10.0, 90.0,
					  10.0, 0.0, 10.0, 100.0));
	
      // Test 5 - colinear nonoverlapping lines
      harness.check(!Line2D.linesIntersect(10.0, 10.0, 10.0, 90.0,
					   10.0, 91.0, 10.0, 100.0));
      
      // Test 6 - zero length lines at same point
      harness.check(Line2D.linesIntersect(1.0, 1.0, 1.0, 1.0,
                      1.0, 1.0, 1.0, 1.0));
      
      // Test 7 - segments share end point
      harness.check(Line2D.linesIntersect(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 1.0, 0.0));
      
  }
}