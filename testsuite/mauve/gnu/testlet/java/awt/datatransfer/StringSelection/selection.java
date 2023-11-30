// Tags: JDK1.1

// Copyright (C) 2005 Mark J. Wielaard <mark@klomp.org>

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
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA.

package gnu.testlet.java.awt.datatransfer.StringSelection;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.datatransfer.*;
import java.io.*;

public class selection implements Testlet
{
  public void test(TestHarness harness)      
  {
    try
      {
	String testString = "Mauve test data string";
	StringSelection ss = new StringSelection(testString);
	
	harness.check(ss.isDataFlavorSupported(DataFlavor.stringFlavor));
	harness.check(ss.isDataFlavorSupported(DataFlavor.plainTextFlavor));
	harness.check(!ss.isDataFlavorSupported(DataFlavor.imageFlavor));
	
	DataFlavor[] dfs = ss.getTransferDataFlavors();
	harness.check(dfs.length, 2);
	harness.check(dfs[0].equals(DataFlavor.stringFlavor)
		      || dfs[0].equals(DataFlavor.plainTextFlavor));
	harness.check(dfs[1].equals(DataFlavor.stringFlavor)
		      || dfs[1].equals(DataFlavor.plainTextFlavor));
	
	String s = (String) ss.getTransferData(DataFlavor.stringFlavor);
	harness.check(s, testString);
	
	// Note that this described in the literature (e.g. Flanagan,
	// Nutshell) as how a StringSelection actually works. Returning a
	// Reader here is of course wrong for a plainTextFlavor, but this
	// is what applications expect.
	Reader r  = (Reader) ss.getTransferData(DataFlavor.plainTextFlavor);
	StringBuffer sb = new StringBuffer();
	int i = r.read();
	while (i != -1)
	  {
	    sb.append((char) i);
	    i = r.read();
	  }
	harness.check(sb.toString(), testString);
      }
    catch (IOException ioe)
      {
	harness.debug(ioe);
	harness.check(false, ioe.toString());
      }
    catch(UnsupportedFlavorException ufe)
      {
	harness.debug(ufe);
	harness.check(false, ufe.toString());
      }
  }
}
