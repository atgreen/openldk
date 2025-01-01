// Tags: JDK1.4

// Copyright (C) 2004 Free Software Foundation, Inc.
// Written by Julian Scheid (julian@sektor37.de) and
// Mark J. Wielaard (mark@klomp.org)

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

import java.nio.*;
import java.nio.charset.*;

/**
 * Test for some under/overflow situations exposing a bug in GNU
 * Classpath UTF_8 Charset implementation found by Julian Scheid
 * (julian@sektor37.de).
 */
public class UTF8Charset implements Testlet
{
  public void test(TestHarness h)
  {
    final int first_chunk_size = 4;
    final int second_chunk_size = 3;
    
    byte[] inBytes = new byte[first_chunk_size + second_chunk_size];
    
    // fill with some harmless ASCII7 char
    for (int i = 0; i < inBytes.length; ++i)
      inBytes[i] = 'X';

    ByteBuffer inBuffer = ByteBuffer.wrap(inBytes);

    CharBuffer outBuffer1 = CharBuffer.allocate(first_chunk_size);
    CharBuffer outBuffer2 = CharBuffer.allocate(second_chunk_size);

    Charset utf8Charset = Charset.forName("UTF-8");
    CharsetDecoder decoder = utf8Charset.newDecoder();

    CoderResult coderResult1
      = decoder.decode(inBuffer, outBuffer1, false);
    
    h.check(coderResult1.isOverflow(),
	    "Expected decoder to return overflow status");
    h.check(first_chunk_size == inBuffer.position(),
	    "Expected input buffer position to be " + first_chunk_size
	    + ", but it is " + inBuffer.position());
    
    CoderResult coderResult2
      = decoder.decode(inBuffer, outBuffer2, false);
    
    h.check(coderResult2.isUnderflow(),
	    "Expected decoder to return underflow status");
    h.check((first_chunk_size + second_chunk_size) == inBuffer.position(),
	    "Expected input buffer position to be "
	    + (first_chunk_size + second_chunk_size)
	    + ", but it is " + inBuffer.position());
  }
}
