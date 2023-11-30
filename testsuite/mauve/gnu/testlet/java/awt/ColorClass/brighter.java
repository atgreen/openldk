/* brighter.java 
   Copyright (C) 2006 RedHat
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

// Tags: 1.3

package gnu.testlet.java.awt.ColorClass;


import java.awt.Color; 

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

  /**
   * MenuBarTest
   */
  public class brighter implements Testlet {

    public void test(TestHarness harness)
    {
      
      for (int i = 0; i <= 3; i++)
        {
          for (int j = 0; j <= 255; j++)
            {
              for (int k = 0; k <= 255; k++)
                {
                  Color c = new Color(i, j, k);
                  Color c2;
                  c2 = c.brighter();
                  int value = c.getRGB();
                  int[] colors = new int[3];
                  colors[0] = (value & (255 << 16)) >> 16;
                  colors[1] = (value & (255 << 8)) >> 8;
                  colors[2] = value & 255;
                  // (0,0,0) is a special case.
                  if (colors[0] == 0 && colors[1] == 0 && colors[2] ==0)
                    {
                      colors[0] = 3;
                      colors[1] = 3;
                      colors[2] = 3;
                    }
                  else
                    {                    
                      for (int index = 0; index < 3; index++)
                        {
                          
                          if (colors[index] > 2)
                            colors[index] = (int) Math.min(255, colors[index]/0.7f);
                          if (colors[index] == 1 || colors[index] == 2)
                            colors[index] = 4;
                        }
                    }
                  harness.check(c2.getRed(), colors[0]);
                  harness.check(c2.getBlue(), colors[2]);
                  harness.check(c2.getGreen(), colors[1]);             
                }
            }
        }
    }
    
  }
