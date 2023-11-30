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

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;

import java.util.Random;
import java.util.Scanner;

/**
 * @author E0327023 Hernadi Laszlo
 * @ 26.02.2007 - 04:15:47
 *
 */
public class Inputs extends Base
{
  /**
   * The amount of long numbers to generate and test.
   */
  private final static int AMOUNT = 20000;

  public Inputs ()
  {
    fileName = getClass().getName () + ".txt";
    this.isEnabled = false;
  }

  /* (non-Javadoc)
   * @see gnu.testlet.java.util.Scanner.TestBase#doTest()
   */
  @Override protected void doTest ()
  {
    long[] numbers = new long[AMOUNT];
    Random r = new Random (System.currentTimeMillis ());
    int i;
    long tmp;
    int errors;
    final long max = 20000000000000L, mean = max >> 1;
    StringBuffer tmpBuff = new StringBuffer (10000);
    FileOutputStream fos = null;
    Scanner s = null;

    String[]charSets = {"windows-1252"};
    String aktName;

    int aktCS;

    for (i = 0; i < numbers.length; i++)
      {
	tmp = ((long) (r.nextFloat () * max) - mean);
	numbers[i] = tmp;
      }
    tmpBuff.insert (0, "" + numbers[0]);

    for (i = 1; i < numbers.length; i++)
      {
	tmpBuff.append (" " + numbers[i]);
      }

    try
    {
      fos = new FileOutputStream (this.aktFile);
      fos.write (tmpBuff.toString ().getBytes ());
      fos.flush ();
      fos.close ();

//                      Scanner s = new Scanner (aktFile);

      myHarness.debug ("Testing Readable input...");
      errors = 0;
      s = new Scanner (new FileReader (this.aktFile));
      i = 0;
      while (s.hasNextLong ())
	{
	  tmp = s.nextLong ();
	  if (tmp != numbers[i])
	    {
	      errors++;
	      this.myHarness.fail ("Readable : nextLong() -> " + tmp +
				   " != " + numbers[i]);
	    }
	  i++;
	}
      if (errors == 0)
	myHarness.debug ("all OK");
      else
	this.myHarness.fail (errors + " errors..");

      myHarness.debug ("Testing ReadableByteChanel input...");
      errors = 0;
      s = new Scanner ((new FileInputStream (this.aktFile)).getChannel ());
      i = 0;
      while (s.hasNextLong ())
	{
	  tmp = s.nextLong ();
	  if (tmp != numbers[i])
	    {
	      errors++;
	      this.myHarness.fail ("ReadableByteChanel : nextLong() -> " +
				   tmp + " != " + numbers[i]);
	    }
	  i++;
	}
      if (errors == 0)
	myHarness.debug ("all OK");
      else
	this.myHarness.fail (errors + " errors..");

      for (aktCS = 0; aktCS < charSets.length; aktCS++)
	{
	  aktName = "Testing File + CharSet \"" + charSets[aktCS] + "\"";
	  myHarness.debug (aktName);
	  errors = 0;
	  s = new Scanner (this.aktFile, charSets[aktCS]);
	  i = 0;
	  while (s.hasNextLong ())
	    {
	      tmp = s.nextLong ();
	      if (tmp != numbers[i])
		{
		  errors++;
		  this.myHarness.fail (aktName + " : nextLong() -> " + tmp +
				       " != " + numbers[i]);
		}
	      i++;
	    }
	  if (errors == 0)
	    myHarness.debug ("all OK");
	  else
	    this.myHarness.fail (errors + " errors..");

	  aktName =
	    "Testing InputStream + CharSet \"" + charSets[aktCS] + "\"";
	  myHarness.debug (aktName);
	  errors = 0;
	  s =
	    new Scanner (new
			   ByteArrayInputStream (tmpBuff.toString ().
						 getBytes ()),
			   charSets[aktCS]);
	  i = 0;
	  while (s.hasNextLong ())
	    {
	      tmp = s.nextLong ();
	      if (tmp != numbers[i])
		{
		  errors++;
		  this.myHarness.fail (aktName + " : nextLong() -> " + tmp +
				       " != " + numbers[i]);
		}
	      i++;
	    }
	  if (errors == 0)
	    myHarness.debug ("all OK");
	  else
	    this.myHarness.fail (errors + " errors..");

	  aktName =
	    "Testing ReadableByteChanel + CharSet \"" + charSets[aktCS] +
	    "\"";
	  myHarness.debug (aktName);
	  errors = 0;
	  s =
	    new Scanner ((new FileInputStream (this.aktFile)).getChannel (),
			   charSets[aktCS]);
	  i = 0;
	  while (s.hasNextLong ())
	    {
	      tmp = s.nextLong ();
	      if (tmp != numbers[i])
		{
		  errors++;
		  this.myHarness.fail (aktName + " : nextLong() -> " + tmp +
				       " != " + numbers[i]);
		}
	      i++;
	    }
	  if (errors == 0)
	    myHarness.debug ("all OK");
	  else
	    this.myHarness.fail (errors + " errors..");

	}

      this.myHarness.check (i, numbers.length,
			    "Incomplete read... (" + i + " / " +
			    numbers.length + ")");
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
