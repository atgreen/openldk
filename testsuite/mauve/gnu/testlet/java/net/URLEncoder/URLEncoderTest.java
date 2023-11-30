// Tags: JDK1.0

/*
   Copyright (C) 1999, 2003 Hewlett-Packard Company

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
   Boston, MA 02111-1307, USA.
*/

package gnu.testlet.java.net.URLEncoder;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.net.*;


public class URLEncoderTest implements Testlet
{
  protected static TestHarness harness;
	public void test_Basics()
	{
		String str1 = URLEncoder.encode("abcdefghijklmnopqrstuvwxyz");
		harness.check (str1, "abcdefghijklmnopqrstuvwxyz",
			"Error : test_Basics - 1 " + 
			" String returned is not encoded properly");

		String str2 = URLEncoder.encode("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
		harness.check (str2, "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
			"Error : test_Basics - 2 " + 
			" String returned is not encoded properly");

		String str3 = URLEncoder.encode("hi there buddy");
		harness.check (str3, "hi+there+buddy",
			"Error : test_Basics - 3 " + 
			" String returned is not encoded properly");

		String str4 = URLEncoder.encode("0123456789:;<");
		harness.check (str4, "0123456789%3A%3B%3C",
			"Error : test_Basics - 4 " + 
			" String returned is not encoded properly");

		String str5 = URLEncoder.encode("\n");
		harness.check (str5, "%0A", "test encoding of \\n");
	}

	public void testall()
	{
		test_Basics();
	}

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}


