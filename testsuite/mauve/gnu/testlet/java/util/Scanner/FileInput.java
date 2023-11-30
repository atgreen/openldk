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

// Tags: JDK1.5

/**
 * 
 */
package gnu.testlet.java.util.Scanner;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import java.util.Scanner;

/**
 * @author E0327023 Hernadi Laszlo
 * @ 14.02.2007 - 12:20:18
 *
 */
public class FileInput extends Base
{
  public FileInput ()
  {
    setDefaultFilename ();
  }

  /* (non-Javadoc)
   * @see gnu.testlet.java.util.Scanner.TestBase#doTest()
   */
   @Override protected void doTest ()
  {
    this.myHarness.checkPoint ("File Input");

    String[]testWords =
    {
    "This", "is", "a", "simple", "Test", "don't", "panik", "it's", "just",
	"a", "Teset"};
    testWords[0] = testWords[0].trim ();
    String testString = testWords[0];
    String tmpStr;
    int i;

    for (i = 1; i < testWords.length; i++)
      {
	testWords[i] = testWords[i].trim ();
	testString += " " + testWords[i].trim ();
      }

    try
    {
      FileOutputStream fos = new FileOutputStream (this.aktFile);
      fos.write (testString.getBytes ());
      fos.flush ();
      fos.close ();
      Scanner s = new Scanner (aktFile);
      i = 0;
      while (s.hasNext ())
	{
	  tmpStr = s.next ();
	  this.myHarness.check (tmpStr, testWords[i],
				"next() -> \"" + tmpStr + "\" != \"" +
				testWords[i] + "\"");
	  // System.out.println("\"" + tmpStr + "\" ?= \"" + testWords[i] + "\"");
	  i++;
	}
      this.myHarness.check (i, testWords.length,
			    "Incomplete read... (" + i + " / " +
			    testWords.length + ")");
      s.close ();
    }
    catch (FileNotFoundException e)
    {
      this.myHarness.fail ("Could not create file");
    }
    catch (IOException e)
    {
      this.myHarness.fail ("Could not write to File \"" +
			   this.aktFile.getName () + "\"");
    }
  }

}
