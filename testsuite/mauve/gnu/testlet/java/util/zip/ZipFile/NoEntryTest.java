// Tags: JDK1.1

// Copyright (C) 2004 Free Software Foundation
// Contributed by Anthony Green <green@redhat.com>

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
// Boston, MA 02111-1307, USA. 

package gnu.testlet.java.util.zip.ZipFile;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.zip.*;
import java.io.*;

public class NoEntryTest implements Testlet
{
    public void test (TestHarness harness)
    {
      boolean pass = false;

      try
	{
	  File temp = File.createTempFile ("NoEntryTest", ".zip");
	  temp.deleteOnExit ();
	  ZipOutputStream zout = 
	    new ZipOutputStream(new FileOutputStream (temp));
	  ZipEntry ze = new ZipEntry ("one");
	  zout.putNextEntry (ze);
	  zout.close();
	  
	  ZipFile zf = new ZipFile (temp);
	  ze = new ZipEntry ("this/does/not/exist");
	  InputStream is = zf.getInputStream (ze);
	  
	  if (is == null)
	    pass = true;
	}
      catch (Exception ex)
	{
	}

      harness.check (pass, "getInputStream for missing ZipEntry returns null");
    }
}
