// mojo.java - Test encode/decode of UTF-8.
// From "Mojo Jojo" <mojojojo@pacbell.net>

/*************************************************************************
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published 
/* by the Free Software Foundation, either version 2 of the License, or
/* (at your option) any later version.
/*
/* This program is distributed in the hope that it will be useful, but
/* WITHOUT ANY WARRANTY; without even the implied warranty of
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/* GNU General Public License for more details.
/*
/* You should have received a copy of the GNU General Public License
/* along with this program; if not, write to the Free Software Foundation
/* Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA
/*************************************************************************/

// Tags: JDK1.1

package gnu.testlet.java.io.Utf8Encoding;

import java.io.*;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Generates some test data and processes it using java.io character
 * conversion support for the UTF-8 encodings.  Gives that character
 * conversion support an overall pass or fail rating.
 *
 * <P> Some of the test cases here are taken from standard XML test suites;
 * UTF-8 is one of the two encodings XML processors must support, so this
 * encoding should be very correct in order to support next generation
 * web (and internet) applications.
 *
 * <P> Note that JDK 1.1 and JDK 1.2 don't currently pass these tests;
 * there are known problems in their UTF-8 encoding support at this time.
 */
public class mojo implements Testlet
{
    //
    // Positive tests -- test both output and input processing against
    // various "known good" data
    //
    private static void positive (
        TestHarness harness,
	byte	encoded [],
	char	decoded [],
	String	label
    ) {
	boolean				flag = true;
	int				i = 0;

	harness.checkPoint (label);
	try {
	    //
	    // Ensure that writing encodes correctly
	    //
	    ByteArrayOutputStream	out;
	    OutputStreamWriter		writer;
	    byte			result [];

	    out = new ByteArrayOutputStream ();
	    writer = new OutputStreamWriter (out, "UTF8");
	    writer.write (decoded);
	    writer.close ();
	    result = out.toByteArray ();

	    harness.check (result.length, encoded.length);
	    flag = true;
	    for (i = 0; i < encoded.length && i < result.length; i++) {
		if (encoded [i] != result [i]) {
		    harness.debug ("failing index = " + i);
		    flag = false;
		}
	    }
	    harness.check (flag);

	    //
	    // Ensure that reading decodes correctly
	    //
	    ByteArrayInputStream	in;
	    InputStreamReader		reader;

	    in = new ByteArrayInputStream (encoded);
	    reader = new InputStreamReader (in, "UTF8");

	    flag = true;
	    for (i = 0; i < decoded.length; i++) {
		int			c = reader.read ();

		harness.check (c, decoded[i]);
		if (c != decoded [i]) {
		    harness.debug (label + ": read failed, char " + i);
		    flag = false;
		    break;
		}
	    }
	    harness.check (flag);

	    // Look for EOF.
	    harness.check (reader.read(), -1);
	} catch (Exception e) {
	    harness.debug (label + ": failed "
		+ "(i = " + i + "), "
		+ e.getClass ().getName ()
		+ ", " + e.getMessage ());
	    // e.printStackTrace ();
	}
	return;
    }


    //
    // Negative tests -- only for input processing, make sure that
    // invalid or corrupt characters are rejected.
    //
    private static void negative (TestHarness harness,
				  byte encoded [], String label)
    {
        boolean flag = false;
	harness.checkPoint (label);
	try {
	    ByteArrayInputStream	in;
	    InputStreamReader		reader;
	    int				c;

	    in = new ByteArrayInputStream (encoded);
	    reader = new InputStreamReader (in, "UTF8");

	    c = reader.read ();
	    flag = (c == 0xFFFD); // Should be replacement char
	} catch (Throwable t) {
	    harness.debug (label + ": failed, threw "
			   + t.getClass ().getName ()
			   + ", " + t.getMessage ());
	}
	harness.check (flag);
    }


    //
    // TEST #0:  Examples from RFC 2279
    // This is a positive test.
    //
    private static byte test0_bytes [] = {
	// A<NOT IDENTICAL TO><ALPHA>.
        (byte)0x41,
	(byte)0xE2, (byte)0x89, (byte)0xA2,
	(byte)0xCE, (byte)0x91,
	(byte)0x2E,
	// Korean word "hangugo"
	(byte)0xED, (byte)0x95, (byte)0x9C,
	(byte)0xEA, (byte)0xB5, (byte)0xAD,
	(byte)0xEC, (byte)0x96, (byte)0xB4,
        // Japanese word "nihongo"
        (byte)0xE6, (byte)0x97, (byte)0xA5,
	(byte)0xE6, (byte)0x9C, (byte)0xAC,
	(byte)0xE8, (byte)0xAA, (byte)0x9E
    };
    private static char test0_chars [] = {
	// A<NOT IDENTICAL TO><ALPHA>.
	0x0041, 0x2262, 0x0391, 0x002e,
	// Korean word "hangugo"
	0xD55C, 0xAD6D, 0xC5B4,
        // Japanese word "nihongo"
	0x65E5, 0x672C, 0x8A9E
    };


    //
    // From RFC 2279, the ranges which define the values we focus some
    // "organized" testing on -- test each boundary, and a little on each
    // side of the boundary.
    //
    // Note that some encodings are errors:  the shortest encoding must be
    // used.  On the "be lenient in what you accept" principle, those not
    // tested as input cases; on the "be strict in what you send" principle,
    // they are tested as output cases instead.
    //
    // UCS-4 range (hex.)           UTF-8 octet sequence (binary)
    // 0000 0000-0000 007F   0xxxxxxx
    // 0000 0080-0000 07FF   110xxxxx 10xxxxxx
    // 0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx
    //
    // 0001 0000-001F FFFF   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    // 0020 0000-03FF FFFF   111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
    // 0400 0000-7FFF FFFF   1111110x 10xxxxxx ... 10xxxxxx
    //

    //
    // TEST #1:  One byte encoded values.  Works just like ASCII; these
    // encodings were chosen for boundary testing.
    // This is a positive test.
    //
    // 0000 0000-0000 007F   0xxxxxxx
    //
    private static byte test1_bytes [] = {
	(byte) 0x00, (byte) 0x01, (byte) 0x7e, (byte) 0x7f
    };
    private static char test1_chars [] = {
	0x0000, 0x0001, 0x007e, 0x007f
    };


    //
    // TEST #2:  Two byte encoded values, chosen for boundary testing.
    // This is a positive test.
    //
    // 0000 0080-0000 07FF   110xxxxx 10xxxxxx
    //
    // Encodings CX bb, with X = 0 or 1 and 'b' values irrelevant,
    // should have used a shorter encoding.
    //
    private static byte test2_bytes [] = {
	(byte) 0xc2, (byte) 0x80,
	(byte) 0xc2, (byte) 0x81,
	(byte) 0xc3, (byte) 0xa0,
	(byte) 0xdf, (byte) 0xbe,
	(byte) 0xdf, (byte) 0xbf
    };
    private static char test2_chars [] = {
	0x0080,
	0x0081,
	0x00E0,
	0x07FE,
	0x07FF
    };


    //
    // TEST #3:  Three byte encoded values, chosen for boundary testing.
    // This is a positive test.
    //
    // 0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx
    //
    // Encodings EO Xb bb, with X = 8 or 9 and 'b' values irrelevant,
    // should have used a shorter encoding.
    //
    private static byte test3_bytes [] = {
	(byte) 0xe0, (byte) 0xa0, (byte) 0x80,
	(byte) 0xe0, (byte) 0xa0, (byte) 0x81,
	// (byte) 0xe0, (byte) 0x11, (byte) 0x10,
	// (byte) 0xe1, (byte) 0x10, (byte) 0x10,
	(byte) 0xef, (byte) 0xbf, (byte) 0xbe,
	(byte) 0xef, (byte) 0xbf, (byte) 0xbf
    };
    private static char test3_chars [] = {
	0x0800,
	0x0801,
	// 0x????,
	// 0x????
	0xFFFE,
	0xFFFF
    };


    //
    // TEST #4:  Four byte encoded values, needing surrogate pairs.
    // This is a positive test.
    //
    // NOTE:  some four byte encodings exceed the range of Unicode
    // with surrogate pairs (UTF-16); those must be negatively tested.
    //
    // 0001 0000-001F FFFF   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    //
    // Encodings F0 8b bb bb, where again the 'b' values are irrelevant,
    // should have used a shorter encoding.
    //
    private static byte test4_bytes [] = {
	(byte) 0xf0, (byte) 0x90, (byte) 0x80, (byte) 0x80,
	(byte) 0xf0, (byte) 0x90, (byte) 0x80, (byte) 0x81,
	(byte) 0xf0, (byte) 0x90, (byte) 0x88, (byte) 0x80,
	(byte) 0xf0, (byte) 0x90, (byte) 0x90, (byte) 0x80,
	(byte) 0xf0, (byte) 0x90, (byte) 0x8f, (byte) 0xbf,
	(byte) 0xf1, (byte) 0x90, (byte) 0x8f, (byte) 0xbf,
	(byte) 0xf2, (byte) 0x90, (byte) 0x8f, (byte) 0xbf,
	(byte) 0xf4, (byte) 0x8f, (byte) 0xbf, (byte) 0xbf
    };
    private static char test4_chars [] = {
	0xD800, 0xDC00,
	0xD800, 0xDC01,
	0xD800, 0xDE00,
	0xD801, 0xDC00,
	0xD800, 0xDFFF,
	0xD900, 0xDFFF,
	0xDA00, 0xDFFF,
	0xDBFF, 0xDFFF,
    };


    //
    // NEGATIVE TESTS:  quadruple byte encodings that are out of range
    // for UTF-16 (Unicode with surrogate pairs); five and six byte
    // encodings (even if they're bogus encodings of 'good' values);
    // and orphan "extension" bytes (e.g. ISO-8859-1 treated as UTF-8,
    // accented and other non-ASCII characters should force errors).
    //
    private static byte test5_bytes []
	= { (byte) 0xf7, (byte) 0x8f, (byte) 0xbf, (byte) 0xbf };
    private static byte test6_bytes []
	= { (byte) 0xf7, (byte) 0x8f, (byte) 0xbf, (byte) 0xbf };
    private static byte test7_bytes []
	= { (byte) 0xf8, (byte) 0x80, (byte) 0x80,
	    (byte) 0x80, (byte) 0x80 };
    private static byte test8_bytes []
	= { (byte) 0xf8, (byte) 0xbf, (byte) 0x80,
	    (byte) 0x80, (byte) 0x80 };
    private static byte test9_bytes []
	= { (byte) 0xfc, (byte) 0x80, (byte) 0x80,
	    (byte) 0x80, (byte) 0x80, (byte) 0x80 };
    private static byte test10_bytes []
	= { (byte) 0xfc, (byte) 0x80, (byte) 0x80,
	    (byte) 0x80, (byte) 0x80, (byte) 0x81 };
    private static byte test11_bytes []
	= { (byte) 0x80 };
    private static byte test12_bytes []
	= { (byte) 0xa9 };
    private static byte test13_bytes []
	= { (byte) 0xf7, (byte) 0x80, (byte) 0x80, (byte) 0x80 };
    

    //
    // Just for information -- see if these cases are accepted; they're
    // all errors ("too short" encodings), but ones which generally
    // ought to be accepted (though see RFC 2279).
    //
	// three encodings of ASCII NUL
    private static byte bad0_bytes []
	= { (byte) 0xc0, (byte) 0x80 };
    private static byte bad1_bytes []
	= { (byte) 0xe0, (byte) 0x80, (byte) 0x80 };
    private static byte bad2_bytes []
	= { (byte) 0xf0, (byte) 0x80, (byte) 0x80, (byte) 0x80 };

	// ... and other values
    private static byte bad3_bytes []
	= { (byte) 0xc1, (byte) 0x80 };
    private static byte bad4_bytes []
	= { (byte) 0xe0, (byte) 0x81, (byte) 0x80 };
    private static byte bad5_bytes []
	= { (byte) 0xe0, (byte) 0x90, (byte) 0x80 };


    /**
     * Main program to give a pass or fail rating to a JVM's UTF-8 support.
     * No arguments needed.
     */
    public void test (TestHarness harness)
    {
	boolean		pass;

	//
	// Positive tests -- good data is dealt with correctly
	//
	positive (harness, test0_bytes, test0_chars, "RFC 2279 Examples");
	positive (harness, test1_bytes, test1_chars, "One Byte Characters");
	positive (harness, test2_bytes, test2_chars, "Two Byte Characters");
	positive (harness, test3_bytes, test3_chars, "Three Byte Characters");
	positive (harness, test4_bytes, test4_chars, "Surrogate Pairs");

	//
	// Negative tests -- "bad" data is dealt with correctly ... in
	// this case, "bad" is just out-of-range for Unicode systems,
	// rather than values encoded contrary to spec (such as NUL
	// being encoded as '0xc0 0x80', not '0x00').
	//
	negative (harness, test5_bytes,  "Four Byte Range Error (0)");
	negative (harness, test6_bytes,  "Four Byte Range Error (1)");
	negative (harness, test7_bytes,  "Five Bytes (0)");
	negative (harness, test8_bytes,  "Five Bytes (1)");
	negative (harness, test9_bytes,  "Six Bytes (0)");
	negative (harness, test10_bytes, "Six Bytes (1)");
	negative (harness, test11_bytes, "Orphan Continuation (1)");
	negative (harness, test12_bytes, "Orphan Continuation (2)");
	negative (harness, test13_bytes, "Four Byte Range Error (2)");

	//
	// Just for information
	//
	// FIXME: for Mauve it is simpler to turn these off.  Bummer.
// 	boolean		strict;

// 	System.out.println ("");
// 	System.out.println ("------ checking decoder leniency ...");

// 	strict  = negative (harness, bad0_bytes, "Fat zero (0)");
// 	strict &= negative (harness, bad1_bytes, "Fat zero (1)");
// 	strict &= negative (harness, bad2_bytes, "Fat zero (2)");
// 	strict &= negative (harness, bad3_bytes, "Fat '@' (0)");
// 	strict &= negative (harness, bad4_bytes, "Fat '@' (1)");
// 	strict &= negative (harness, bad5_bytes, "Fat 0x0400");

// 	if (strict)
// 	    System.out.println ("... decoder is strict.");
// 	else
// 	    System.out.println ("... decoder is lenient.");
    }
}
