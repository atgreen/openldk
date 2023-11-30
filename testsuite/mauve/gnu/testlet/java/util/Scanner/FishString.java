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

import gnu.testlet.Testlet;

import java.util.Scanner;

import java.util.regex.MatchResult;

/**
 * @author E0327023 Hernadi Laszlo @ 12.02.2007 - 23:00:30
 */
public class FishString extends Base
{

  @Override protected void doTest ()
  {
    String input = "1 fish 2 fish red fish blue fish";
    String delimiter = "\\s*fish\\s*";
    String tmpStr;
      String[] values =
    {
    null, "1", "2", "red", "blue"};	// the null is required...
    int i;

    this.myHarness.checkPoint ("Fisch String [" + input + "]");
    Scanner s1 = new Scanner (input);
    s1.useDelimiter (delimiter);
    tmpStr = s1.delimiter ().pattern ();
    this.myHarness.check (tmpStr, delimiter,
			  "get / setDelimiter fail (\"" + tmpStr +
			  "\" != \"" + delimiter);
    this.myHarness.check (s1.hasNext (), true, "first hasNext()");
    this.myHarness.check (s1.hasNextInt (), true, "first hasNextInt()");
    this.myHarness.check (s1.nextInt (), 1, "nextInt()");
    this.myHarness.check (s1.hasNextInt (), true, "hasNextInt()");
    this.myHarness.check (s1.hasNextBoolean (), false, "hasNextBoolean()");
    this.myHarness.check (s1.hasNextByte (), true, "hasNextByte()");
    this.myHarness.check (s1.nextInt (), 2, "2. nextInt()");
    this.myHarness.check (s1.hasNext (), true, "3. hasNext()");
    this.myHarness.check (s1.hasNextBigInteger (), false,
			  "hasNextBigInteger()");
    this.myHarness.check (s1.next (), "red", "3. next()");
    this.myHarness.check (s1.next (), "blue", "4. next()");
    this.myHarness.check (s1.hasNext (), false, "letztes hasNext()");
    s1.close ();

    // Scanner s2 = new Scanner(input);
    Scanner s2 = new Scanner (input);
    s2.findInLine ("(\\d+) fish (\\d+) fish (\\w+) fish (\\w+)");
    MatchResult mResult = s2.match ();
    for (i = 1; i <= mResult.groupCount (); i++)
      {
	this.myHarness.check (mResult.group (i), values[i],
			      "wrong result : \"" + mResult.group (i) +
			      "\" != \"" + values[i] + "\"");
	// System.out.println(mResult.group(i));
      }
    if (i != values.length)
      {
	this.myHarness.fail ("not all results... (" + i + " != " +
			     (values.length) + ")");
      }
    s2.close ();
  }
}
