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

public class ToASCIIStringTest 
  implements Testlet
{

  private static final String TEST_URI_1 = "http://example.com/money/\uFFE5/file.html";

  public void test(TestHarness h)
  {
    try
      {
	h.check(new URI(TEST_URI_1).toString(), TEST_URI_1);
	h.check(new URI(TEST_URI_1).toASCIIString(), "http://example.com/money/%EF%BF%A5/file.html");
      }
    catch (URISyntaxException e)
      {
	h.debug(e);
	h.fail("Unexpected exception");
      }
  }
}
