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

package gnu.testlet.javax.print.SimpleDoc;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.print.DocFlavor;
import javax.print.SimpleDoc;
import javax.print.attribute.DocAttributeSet;
import javax.print.attribute.HashDocAttributeSet;
import javax.print.attribute.UnmodifiableSetException;
import javax.print.attribute.standard.Compression;
import javax.print.attribute.standard.OrientationRequested;
import javax.print.attribute.standard.Sides;

/**
 * Tests various aspects of the getAttributes method.
 */
public class getAttributes implements Testlet
{
  public void test(TestHarness harness)
  {
    HashDocAttributeSet set = new HashDocAttributeSet();
    set.add(Sides.DUPLEX);
    set.add(Compression.COMPRESS);
    set.add(OrientationRequested.LANDSCAPE);
    
    SimpleDoc doc = 
      new SimpleDoc(new byte[100], DocFlavor.BYTE_ARRAY.GIF, set);
    
    DocAttributeSet set1 = doc.getAttributes();
    DocAttributeSet set2 = doc.getAttributes();
    
    // everytime the same object needs to be returned.
    harness.check(set1 == set2);
    
    try
      {
        // it must be an unmodifiable view
        set1.remove(Compression.class);
        harness.check(false);
      }
    catch (UnmodifiableSetException e)
      {
        harness.check(true);
      }
  }

}
