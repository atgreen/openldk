// Copyright (c) 2007 Hernadi Laszlo

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

// Tags: not-a-test

package gnu.testlet.java.util.Scanner;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.File;

/**
 * @author E0327023 Hernadi Laszlo @ 14.02.2007 - 12:13:06
 */
public abstract class Base implements Testlet
{
  protected TestHarness myHarness = null;

  protected final boolean doCleanUP = false;

  protected String fileName = null;

  protected File aktFile;

  protected boolean isEnabled = true;
  protected boolean forceAll = true;

  public void test (final TestHarness harness)
  {
    if (!isEnabled && !forceAll)
      {
	System.out.println ("\t\tTest Disabled...");
	return;
      }

    this.myHarness = harness;
    if (this.fileName != null)
      {
	this.aktFile = new File (myHarness.getTempDirectory() +
				 myHarness.getSeparator() +
				 this.fileName);
	myHarness.debug("Using file: " + aktFile.toString());
	if (this.doCleanUP)
	  {
	    this.aktFile.deleteOnExit ();
	  }
      }
    try
    {
      doTest ();
    }
    catch (Throwable e)
    {
      e.printStackTrace ();
      myHarness.fail(e.toString());
    }
  }

  protected void setDefaultFilename ()
  {
    this.fileName = this.getClass ().getName () + ".txt";
  }

  protected abstract void doTest ();
}
