// Tags: JDK1.4

/*
   Copyright (C) 2005 Andrew John Hughes (gnu_andrew@member.fsf.org)

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

package gnu.testlet.java.net.URI;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.net.URI;
import java.net.URISyntaxException;

public class ToStringTest 
  implements Testlet
{

  private static final String TEST_URI_1 = "http://example.com/examples?name=Fred#";
  private static final String TEST_URI_2 = "http://example.com/examples?name=Fred";
  private static final String TEST_URI_3 = "http://example.com/examples?";
  private static final String TEST_URI_4 = "http://example.com/examples";
  private static final String TEST_URI_5 = "://example.com/examples";
  private static final String TEST_URI_6 = "//example.com/examples";
  private static final String TEST_URI_7 = "http:///examples";
  private static final String TEST_URI_8 = "http:/examples";

  public void test(TestHarness h)
  {
    try
      {
	URI test1 = new URI(TEST_URI_1);
	h.check(test1.toString(), TEST_URI_1);
	h.check(test1.getRawFragment(), "");
	URI test2 = new URI(TEST_URI_2);
	h.check(test2.toString(), TEST_URI_2);
	h.check(test2.getRawFragment(), null);
	URI test3 = new URI(TEST_URI_3);
	h.check(test3.toString(), TEST_URI_3);
	h.check(test3.getRawQuery(), "");
	URI test4 = new URI(TEST_URI_4);
	h.check(test4.toString(), TEST_URI_4);
	h.check(test4.getRawQuery(), null);
	URI test5 = new URI(TEST_URI_5);
	h.check(test5.toString(), TEST_URI_5);
	h.check(test5.getScheme(), null); // Scheme is different and can't be "".
	URI test6 = new URI(TEST_URI_6);
	h.check(test6.toString(), TEST_URI_6);
	h.check(test6.getScheme(), null);
	URI test7 = new URI(TEST_URI_7);
	h.check(test7.toString(), TEST_URI_7);
	h.check(test7.getRawAuthority(), "");
	URI test8 = new URI(TEST_URI_8);
	h.check(test8.toString(), TEST_URI_8);
	h.check(test8.getRawAuthority(), null);
      }
    catch (URISyntaxException e)
      {
	h.debug(e);
	h.fail("Unexpected exception");
      }
  }
}
