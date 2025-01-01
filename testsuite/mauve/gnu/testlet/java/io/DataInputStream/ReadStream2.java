/*************************************************************************
/* ReadStream2.java -- To a DataInput test from a stream
/*
/* Copyright (c) 1998, 1999 Free Software Foundation, Inc.
/* Written by Daryl Lee <dol@sources.redhat.com>
/* Shameless ripoff of ReadStream.java by Aaron M. Renn (arenn@urbanophile.com)
/*
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

// Tags: not-a-test

// This test includes tests not performed in ReadStream

package gnu.testlet.java.io.DataInputStream;

import java.io.*;
import gnu.testlet.TestHarness;

// Read data previously prepared

public class ReadStream2
{

// NOTE same function is in gnu.testlet.java.io.DataOutputStream.WriteRead2
// Please change it in that place to if you change it here.
public static void
runReadTest(DataInputStream dis, TestHarness harness)
{
  String s2 = "Random";
  byte[] b2 = new byte[s2.length()];
  String s3 = " String Two";
  byte[] b3 = new byte[s3.length()];

  try
    {
	  dis.skipBytes(34);  // skip over "writeChars(Random String One)"
	  dis.readFully(b2);  // get "Random"
	  harness.check(s2, new String(b2), "readFully(buf)");
	  dis.readFully(b3, 0, b3.length);  // get " String Two"
	  harness.check(s3, new String(b3), "readFully(buf, off, len)");
	  dis.read(b2, 0, 1); 
	  harness.check('X', b2[0], "read(b[])");
	  dis.read(b2, 0, 6); 
	  String s4 = new String(b2);
      harness.check("abcdef", s4, "read(b, off, len)");
      harness.check(12, dis.readUnsignedByte(), "readUnsignedByte()");
      harness.check(1234, dis.readUnsignedShort(), "readUnsignedShort()");
   }
  catch (IOException e)
    {
      harness.debug(e);
      harness.check(false, "Reading DataInputStream (2)");
    }
}

} // class ReadStream

