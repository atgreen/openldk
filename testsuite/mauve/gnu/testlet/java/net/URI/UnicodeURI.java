// Tags: JDK1.4

/*
   Copyright (C) 2005 Dalibor Topic <robilad@kaffe.org>

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

public class UnicodeURI implements Testlet
{

    private static final String LATIN_SMALL_LETTER_C_WITH_ACUTE = "\u0107";

    public void test(TestHarness harness)
    {
	/* Check if a URI with Unicode characters is created correctly,
	 * without swallowing characters outside the basic plane.
	 */
	try {
	    final URI uri = new URI(null, LATIN_SMALL_LETTER_C_WITH_ACUTE, null);
	    final String uri_string = uri.toString();
	    harness.check(LATIN_SMALL_LETTER_C_WITH_ACUTE.equals(uri_string));
	}
	catch (URISyntaxException e) {
	    harness.debug(e);
	    harness.fail("unexpected exception" + e.toString());
	}
    }
}
