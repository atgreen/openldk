// Tags: JDK1.4
// Uses: UnicodeBase CharInfo

/* Copyright (C) 1999 Artur Biesiadowski
   Copyright (C) 2004 Stephen Crawley 

This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.lang.Character;

import java.io.*;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.ResourceNotFoundException;

/*
  MISSING:
  Instance tests
  (constructor, charValue, serialization): should be in other file
*/

public class unicode extends UnicodeBase implements Testlet
{

  public unicode() 
  {
    super();
  }

  public unicode(TestHarness aHarness, String filename) 
    throws IOException, ResourceNotFoundException
  {
    super(aHarness, filename);
  }


  public void test(TestHarness harness)
  {
    String fileName = "UnicodeData-4.0.0.txt";
    long start = System.currentTimeMillis();
    try
      {
	unicode t = new unicode(harness, fileName);
	long midtime = System.currentTimeMillis();
	t.performTests();
	harness.debug("Benchmark : load:" + (midtime-start) + 
		      "ms   tests:" +  
		      (System.currentTimeMillis() - midtime) + "ms");
      }
    catch (Exception e)
      {
	harness.debug(e);
	harness.check(false);
      }
  }
}
