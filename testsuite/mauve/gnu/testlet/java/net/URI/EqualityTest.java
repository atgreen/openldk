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

public class EqualityTest 
  implements Testlet
{

  private static final String REL_URI = "..";
  private static final String HIER_URI = "http://jones@example.com:98?name=Fred#1";
  private static final String OPAQ_URI = "isbn:123456789#34";

  public void test(TestHarness h)
  {
    try
      {
	h.check(new URI(REL_URI).equals(new URI(REL_URI)));
	URI testURI = new URI(HIER_URI);
	h.check(testURI.equals(testURI));
	h.check(testURI.equals(new URI(HIER_URI)));
	h.check(!testURI.equals(new URI("ftp://jones@example.com:98?name=Fred#1")));
	h.check(!testURI.equals(new URI("http://jones@example.com:98?name=Fred#2")));
	h.check(!testURI.equals(new URI("http://alice@example.com:98?name=Fred#1")));
	h.check(!testURI.equals(new URI("http://jones@examples.com:98?name=Fred#1")));
	h.check(!testURI.equals(new URI("http://jones@example.com:99?name=Fred#1")));
	h.check(!testURI.equals(new URI("http://jones@example.com:98?name=Sally#1")));
	URI opaqURI = new URI(OPAQ_URI);
	h.check(opaqURI.equals(opaqURI));
	h.check(opaqURI.equals(new URI(OPAQ_URI)));
	h.check(!opaqURI.equals(testURI));
	h.check(!opaqURI.equals(new URI("isbn:987654321#34")));
      }
    catch (URISyntaxException e)
      {
	h.debug(e);
	h.fail("Unexpected exception");
      }
  }
}
