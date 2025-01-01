// Test for OutputStreamWriter methods

// Written by Daryl Lee (dol@sources.redhat.com)
// Elaboration of except.java  by paul@dawa.demon.co.uk

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

// Tags: JDK1.1

package gnu.testlet.java.io.OutputStreamWriter;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.OutputStreamWriter;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class jdk11 implements Testlet
{
  public void test (TestHarness harness)
  {
    try
      {
		String tstr = "ABCDEFGH";
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		OutputStreamWriter osw = new OutputStreamWriter (baos);  //Default encoding
		harness.check(true, "OutputStreamWriter(writer)");
		harness.check(osw.getEncoding() != null,
				"non-null getEncoding");

		osw.write(tstr.charAt(0));					// 'A'
		harness.check(true,"write(int)");
		osw.write("ABCDE", 1, 3);					// 'ABCD'
		harness.check(true,"write(string, off, len)");
		char[] cbuf = new char[8];
		tstr.getChars(4, 8, cbuf, 0);
		osw.write(cbuf, 0, 4);						// 'ABCDEFGH'
		harness.check(true,"write(char[], off, len)");
		osw.flush();
		harness.check(true, "flush()");
		harness.check(baos.toString(), tstr, "Wrote all characters okay");	
		osw.close ();
		harness.check(osw.getEncoding(), null,
				"null encoding after close");
		ByteArrayOutputStream baos2 = new ByteArrayOutputStream();
		// ISO-8859-1 is a required encoding and this is the 
                // "preferred" name, latin1 is a legal alias
                // see also http://www.iana.org/assignments/character-sets
		OutputStreamWriter osw2 = new OutputStreamWriter(baos2, "ISO-8859-1");
		// Note that for java.io the canonical name as returned by
		// getEncoding() must be the "historical" name. ISO8859_1.
		harness.check(osw2.getEncoding(), "ISO8859_1", "OutputStreamWriter(writer, encoding)");
		osw2.close ();
		osw2 = new OutputStreamWriter(baos2, "latin1");
		harness.check(osw2.getEncoding(), "ISO8859_1", "OutputStreamWriter(writer, encoding) // alias");
		osw2.close ();

      }
    catch (IOException e)
      {
		harness.check(false, "IOException unexpected");
      }
  }
}
