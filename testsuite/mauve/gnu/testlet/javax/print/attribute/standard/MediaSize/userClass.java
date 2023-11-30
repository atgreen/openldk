//Tags: JDK1.4

//Copyright (C) 2006 Free Software Foundation, Inc.
//Written by Wolfgang Baer (WBaer@gmx.de)

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
//the Free Software Foundation, 51 Franklin Street, Fifth Floor,
//Boston, MA, 02110-1301 USA.

package gnu.testlet.javax.print.attribute.standard.MediaSize;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.print.attribute.Size2DSyntax;
import javax.print.attribute.standard.MediaSize;
import javax.print.attribute.standard.MediaSizeName;

/**
 * Test that user constructed MediaSize subclass objects
 * are added to the media cache used by static findMedia
 * and getMediaSizeForName methods.
 */
public class userClass implements Testlet
{
  /** MediaSize subclass for test */
  public class MyMediaSize extends MediaSize
  {
    public MyMediaSize(float x, float y, int units)
    {
      super(x, y, units);
    }    
  }
  
  public void test(TestHarness harness)
  {
    // One with name is found as "best" match
    MediaSizeName name = 
      MediaSize.findMedia(111f, 222f, Size2DSyntax.INCH);
    harness.check(name == MediaSizeName.JIS_B0);

    // Register a user MediaSize object
    MyMediaSize myMediaSize = 
      new MyMediaSize(111f, 222f, Size2DSyntax.INCH);

    // Now if it is added to the cache it must be found by 
    // findMedia as it is the exact match
    MediaSizeName name2 = 
      MediaSize.findMedia(111f, 222f, Size2DSyntax.INCH);
    harness.check(name2 == null);
  }

}
