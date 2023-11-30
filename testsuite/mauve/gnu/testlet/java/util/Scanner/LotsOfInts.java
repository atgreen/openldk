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
// Tags: JDK1.5

package gnu.testlet.java.util.Scanner;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import java.util.Random;
import java.util.Scanner;

public class LotsOfInts extends Base
{
  public LotsOfInts ()
  {
    this.isEnabled = false;
    this.fileName = this.getClass ().getName () + ".txt";
  }

  @Override protected void doTest ()
  {
    this.myHarness.checkPoint ("Reading lots of positive ints");

    int[] numbers = new int[100000];
    Random r = new Random (System.currentTimeMillis ());
    int i;
    int tmp;
    final int max = 1000000;
    StringBuilder tmpBuff = new StringBuilder (10000);
    FileOutputStream fos = null;

    for (i = 0; i < numbers.length; i++)
      {
	tmp = (int) (r.nextFloat () * max);
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

      Scanner s = new Scanner (this.aktFile);
      i = 0;
      while (s.hasNextInt ())
	{
	  tmp = s.nextInt ();
	  this.myHarness.check (tmp, numbers[i],
				"nextInt() -> " + tmp + " != " + numbers[i]);
	  i++;
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
