/* clip.java -- some checks for the clip() method in the Graphics2D class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

package gnu.testlet.java.awt.Graphics2D;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;

public class clip implements Testlet
{
  public void test(TestHarness harness)
  {
    testBufferedImageGraphics2D(harness);
  }
  
  public void testBufferedImageGraphics2D(TestHarness harness)
  {
    harness.checkPoint("BufferedImage Graphics2D");
    BufferedImage image = new BufferedImage(100, 80, 
            BufferedImage.TYPE_INT_ARGB);
    Graphics2D g2 = image.createGraphics();
    harness.check(g2.getClip(), null);
    g2.clip(new Rectangle(1, 2, 3, 4));
    harness.check(g2.getClip(), new Rectangle(1, 2, 3, 4));
    g2.clip(new Rectangle(0, 1, 3, 2));
    harness.check(g2.getClip(), new Rectangle(1, 2, 2, 1));
    
    g2.setClip(null);
    g2.translate(10, 20);
    g2.clip(new Rectangle(1, 2, 3, 4));
    harness.check(g2.getClip(), new Rectangle(1, 2, 3, 4));
    
    // try a null argument - the API specification says that this will clear
    // the clip, but in fact the reference implementation throws a 
    // NullPointerException - see the following entry in the bug parade:
    // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6206189
    boolean pass = false;
    try
      {
        g2.clip(null);  
      }
    catch (NullPointerException e)
      {
        pass = true;
      }
    harness.check(pass);
  }
}
