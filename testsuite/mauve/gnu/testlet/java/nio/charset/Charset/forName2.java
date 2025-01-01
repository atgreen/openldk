// Tags: JDK1.4

// Copyright (C) 2004, 2005 Free Software Foundation, Inc.
// Written by Michael Koch (konqueror@gmx.de)
// Adapted by Robert Schuster (thebohemian@gmx.net)

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.nio.charset.Charset;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.nio.charset.*;

public class forName2 implements Testlet {
	private void checkCharset(TestHarness h, String name) {
		boolean supported = false;

		try {
			Charset cs = Charset.forName(name);

			if (cs != null)
				supported = true;
		} catch (Throwable t) {
			// Ignore.
		}

		h.check(supported, "Charset '" + name + "' supported");
	}

	public void test(TestHarness h) {
		/*
		 * Check for standard encodings using case-insensitive and alternative
		 * names.
		 */

		// IANA name for UTF-8
		checkCharset(h, "uTf-8");

		// UTF-8 names from
		// http://java.sun.com/j2se/1.5.0/docs/guide/intl/encoding.doc.html
		checkCharset(h, "utf8");

		checkCharset(h, "UtF-16bE");
		checkCharset(h, "uTf-16Le");

		// IANA names for 8859_1
		checkCharset(h, "IsO-iR-100");
		checkCharset(h, "iSo_8859-1");
		checkCharset(h, "LATIN1");
		checkCharset(h, "L1");
		checkCharset(h, "IbM819");
		checkCharset(h, "cp819");
		checkCharset(h, "CSisolATIN1");

		// IANA names for US-ASCII
		checkCharset(h, "iSo-Ir-6");
		checkCharset(h, "AnSi_X3.4-1986");
		checkCharset(h, "IsO_646.IRV:1991");
		checkCharset(h, "AsCiI");
		checkCharset(h, "IsO646-us");
		checkCharset(h, "Us");
		checkCharset(h, "IbM367");
		checkCharset(h, "cP367");
		checkCharset(h, "CSASCII");

		// UTF-8 names from
		// http://oss.software.ibm.com/cgi-bin/icu/convexp?s=ALL
		/* These fail on official implementation of <= 1.5 */
		checkCharset(h, "ibm-1208");
		checkCharset(h, "ibm-1209");
		checkCharset(h, "ibm-5304");
		checkCharset(h, "ibm-5305");
		checkCharset(h, "windows-65001");
		checkCharset(h, "cp1208");
	}
}
