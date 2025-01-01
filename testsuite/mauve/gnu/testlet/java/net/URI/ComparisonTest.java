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

import java.util.SortedSet;
import java.util.TreeSet;

public class ComparisonTest 
  implements Testlet
{

  private static final String REL_URI = "..";
  private static final String HIER_URI = "http://jones@example.com:98?name=Fred#1";
  private static final String OPAQ_URI = "isbn:123456789#34";

  public void test(TestHarness h)
  {
    try
      {
	h.check(new URI(REL_URI).compareTo(new URI(REL_URI)) == 0);
	URI testURI = new URI(HIER_URI);
	h.check(testURI.compareTo(testURI) == 0);
	h.check(testURI.compareTo(new URI(HIER_URI)) == 0);
	h.check(testURI.compareTo(new URI("ftp://jones@example.com:98?name=Fred#1")) > 0);
	h.check(testURI.compareTo(new URI("http://jones@example.com:98?name=Fred#2")) < 0);
	h.check(testURI.compareTo(new URI("http://alice@example.com:98?name=Fred#1")) > 0);
	h.check(testURI.compareTo(new URI("http://jones@examples.com:98?name=Fred#1")) < 0);
	h.check(testURI.compareTo(new URI("http://jones@example.com:99?name=Fred#1")) < 0);
	h.check(testURI.compareTo(new URI("http://jones@example.com:98?name=Sally#1")) < 0);
	URI opaqURI = new URI(OPAQ_URI);
	h.check(opaqURI.compareTo(opaqURI) == 0);
	h.check(opaqURI.compareTo(new URI(OPAQ_URI)) == 0);
	h.check(opaqURI.compareTo(testURI) > 0);
	h.check(opaqURI.compareTo(new URI("isbn:987654321#34")) < 0);
	SortedSet s = new TreeSet();
	s.add(opaqURI);
	s.add(testURI);
	s.add(new URI("ftp://jones@example.com:98?name=Fred#1"));
	s.add(new URI("http://jones@example.com:98?name=Fred#2"));
	s.add(new URI("http://alice@example.com:98?name=Fred#1"));
	s.add(new URI("http://jones@examples.com:98?name=Fred#1"));
	s.add(new URI("http://jones@example.com:99?name=Fred#1"));
	s.add(new URI("http://jones@example.com:98?name=Sally#1"));
	s.add(new URI("isbn:987654321#34"));
	h.debug(s.toString());
      }
    catch (URISyntaxException e)
      {
	h.debug(e);
	h.fail("Unexpected exception");
      }
  }
}
