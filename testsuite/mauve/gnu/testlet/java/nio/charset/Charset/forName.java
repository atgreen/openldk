// Tags: JDK1.4

// Copyright (C) 2004 Free Software Foundation, Inc.
// Written by Michael Koch (konqueror@gmx.de)

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
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.nio.charset.Charset;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.nio.charset.*;

public class forName implements Testlet
{
  private void checkCharset(TestHarness h, String name)
  {
    boolean supported = false;
    
    try
      {
	Charset cs = Charset.forName(name);
	
	if (cs != null)
	  supported = true;
      }
    catch (Throwable t)
      {
	// Ignore.
      }

    h.check(supported, "Charset '" + name + "' supported");
  }
  
  public void test(TestHarness h)
  {
    // Check for non-existant encodings.
    boolean works = false;

    try
      {
	Charset cs = Charset.forName("foobar");
      }
    catch (UnsupportedCharsetException e)
      {
	works = true;
      }

    h.check(works, "UnsupportedCharsetException expected");
	  
    // Checks for standard encodings.
    checkCharset(h, "ISO-8859-1");
    checkCharset(h, "US-ASCII");
    checkCharset(h, "UTF-8");
    checkCharset(h, "UTF-16");
    checkCharset(h, "UTF-16BE");
    checkCharset(h, "UTF-16LE");
    
    /* Checks for IllegalArgumentException being thrown
     * when given charset name is null.
     */
    works = false;
    try {
    	Charset.forName(null);
    } catch(Exception e) {
    	works = e instanceof IllegalArgumentException;
    }
    h.check(works, "IllegalArgumentException thrown");
  }
  
}
