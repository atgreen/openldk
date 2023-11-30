/* loadImage.java -- some checks for the loadImage() method in the 
       SimpleBeanInfo class.
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
// Uses: MySimpleBeanInfo
// Files: testImage1.gif
 
package gnu.testlet.java.beans.SimpleBeanInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Panel;
import java.awt.image.ImageObserver;

public class loadImage implements Testlet, ImageObserver
{
  public boolean imageUpdate(Image img, int infoflags, int x, int y, int width, int height) {
        // TODO Auto-generated method stub
        return false;
    }

public void test(TestHarness harness)
  {
    MySimpleBeanInfo i = new MySimpleBeanInfo();
    Image image = i.loadImage("testImage1.gif");
    MediaTracker mt = new MediaTracker(new Panel());
    mt.addImage(image, 0);
    try
    {
      mt.waitForAll();
    }
    catch (InterruptedException e)
    {
      e.printStackTrace();
    }
    harness.check(!mt.isErrorAny());
    harness.check(image.getWidth(null), 2);
    harness.check(image.getHeight(null), 2);
    harness.check(i.loadImage("someImageThatDoesNotExist.png"), null);
    harness.check(i.loadImage(null), null);
  }
}
