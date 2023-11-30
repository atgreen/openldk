/*************************************************************************
/* ReadStream.java -- To a DataInput test from a stream
/*
/* Copyright (c) 1998, 1999 Free Software Foundation, Inc.
/* Written by Aaron M. Renn (arenn@urbanophile.com)
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

package gnu.testlet.java.io.DataInputStream;

import java.io.*;
import gnu.testlet.TestHarness;

// Write some data using DataOutput and read it using DataInput.

public class ReadStream
{

// NOTE: Same function is in gnu.testlet.java.io.DataOutputStream.WriteRead
// Please change that copy when you change this copy
public static void
runReadTest(DataInputStream dis, TestHarness harness)
{
  try
    {
      harness.check(dis.readBoolean(), "readBoolean() true");
      harness.check(!dis.readBoolean(), "readBoolean() false");
      harness.check(dis.readByte(), 8, "readByte()");
      harness.check(dis.readByte(), -122, "readByte()");
      harness.check(dis.readChar(), 'a', "readChar()");
      harness.check(dis.readChar(), '\uE2D2', "readChar()");
      harness.check(dis.readShort(), 32000, "readShort()");
      harness.check(dis.readInt(), 8675309, "readInt()");
      harness.check(dis.readLong(), 696969696969L, "readLong()");
      harness.check(Float.toString(dis.readFloat()), "3.1415", "readFloat()");
      harness.check(dis.readDouble(), 999999999.999, "readDouble");
      harness.check((String)dis.readUTF(),
          "Testing code is such a boring activity but it must be done",
          "readUTF()");
      harness.check(dis.readUTF(), "a-->\u01FF\uA000\u6666\u0200RRR",
                    "readUTF()");
    }
  catch (IOException e)
    {
      harness.debug(e);
      harness.check(false, "Reading DataInputStream");
    }
}

} // class ReadStream

