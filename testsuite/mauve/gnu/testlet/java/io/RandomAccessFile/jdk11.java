/*************************************************************************
/* jdk11.java -- java.io.File 1.1 tests
/*
/* Copyright (c) 2001, 2002 Free Software Foundation, Inc.
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

// Tags: JDK1.1
package gnu.testlet.java.io.RandomAccessFile;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.EOFException;
import java.io.File;
import java.io.FileDescriptor;
import java.io.IOException;
import java.io.RandomAccessFile;

public class jdk11
  implements Testlet
{
  public void test (TestHarness harness)
  {
    String fname = "raftmpfile";
    RandomAccessFile raf;
    int rdcnt;
    byte[] buf = {0, 0, 0, 0};


    // Start by deleting test file, if it exists, 
    //	to clear out any leftover data
    File f = new File(fname);

    if (f.exists())
      f.delete();

    // new RandomAccessFile(filename, mode);
    try
    {
      raf = new RandomAccessFile(fname, "rw");
    }
    catch (IOException e)
    {
      harness.fail("new RandomAccessFile(Filename, mode): Can't open file "
                   + fname);

      return; // can't proceed without open file
    }

    try
    {
      FileDescriptor fd = raf.getFD();
    }
    catch (IOException e)
    {
      harness.fail("getFD(): Can't get FileDescriptor");

      return; // shouldn't proceed if no FileDescriptor
    }

    // Test skipBytes
    String teststr = "foobar";
    int testlength = teststr.length();
    byte[] testbytes = new byte[testlength];
    ;

    for (int i = 0; i < teststr.length(); i++)
      testbytes[i] = (byte) teststr.charAt(i);

    try
    {
      // write(b[])	
      raf.write(testbytes);
      harness.check(testlength, raf.length(), "write(b[])/length()");
      harness.debug("File size = " + raf.length() + "; should = " + testlength);
      raf.seek(0);

      // Make sure skipBytes goes all the way to the end of the file and no further
      int skipped = 0;

      for (int i = 0; i < testbytes.length + 1; i++)
      { // last skip should return 0 bytes
        int offset = raf.skipBytes(1);
        harness.debug("skipped " + offset + " bytes");
        skipped += offset;
      }

      harness.check(skipped, testlength, "skipBytes() did not skip past EOF");

      // read()
      raf.seek(0);

      char ch1 = (char) raf.read();
      char ch2 = teststr.charAt(0);
      harness.check(ch1, ch2, "read()");
      harness.debug("Read " + ch1 + "; should be " + ch2);

      // getFilePointer()  (sneak this one in because all the setup is done already)
      harness.check(raf.getFilePointer(), 1, "getFilePointer()");

      // read(b[], off, len)
      raf.seek(0); // test seek and read multiple bytes all at once
      rdcnt = raf.read(buf, 0, 3);
      harness.check(rdcnt, 3,
                    "read(b[], off, len):Reading correct number of bytes");
      harness.debug("Read " + rdcnt + " bytes; should have been 3.");

      String str = new String(buf);
      harness.check(str.substring(0, 3).equals(teststr.substring(0, 3)),
                    "read(b[], off, len):Reading at correct offset");
      harness.debug("array read: read " + str + "; expected "
                    + teststr.substring(0, 3));

      // read(b[])
      raf.seek(0);
      rdcnt = raf.read(buf);
      harness.check(rdcnt, buf.length, "read(b[])");
      harness.debug("buffer fill: read " + str + "; expected "
                    + teststr.substring(0, 3));

      // readFully(b[])
      int buf2ln = teststr.length() + 5; // make a buffer big enough to hold all the data
      byte[] buf2 = new byte[buf2ln];

      for (int i = 0; i < buf2ln; i++)
        buf2[i] = 0; // fill with zeroes so we can test length

      raf.seek(0);

      try
      {
        raf.readFully(buf2);
      }
      catch (EOFException eofe)
      {
        harness.check(buf2[testlength - 1],
                      teststr.charAt(teststr.length() - 1),
                      "readFully(b[]):Enough bytes read");
        harness.check(buf2[testlength], 0, "readFully(b[]):Too many bytes");
      }

      // readFully(b[], off, len)
      for (int i = 0; i < buf2ln; i++)
        buf2[i] = 0; // fill with zeroes so we can test length

      raf.seek(0);

      try
      {
        raf.readFully(buf2, 0, testlength + 2);
      }
      catch (EOFException eofe)
      {
        harness.check(buf2[testlength - 1],
                      teststr.charAt(teststr.length() - 1),
                      "readFully(b[],off,len):Enough bytes read");
        harness.check(buf2[testlength], 0,
                      "readFully(b[],off,len):Too many bytes");
      }

      // write(b[], off, len);
      raf.seek(0);
      raf.write(testbytes, 2, 3);
      raf.seek(0);
      raf.read(buf2, 0, 3);

      String t1;
      String b1;
      t1 = new String(testbytes, 2, 3);
      b1 = new String(buf2, 0, 3);
      harness.check(t1, b1, "write(b[], off, len");
      harness.debug("write(b[], off, len):Wrote " + t1 + ", read " + b1);

      // write(byte)/writeByte(byte)/readByte()
      raf.seek(0);
      raf.write(12);
      raf.seek(0);
      harness.check(raf.readByte(), 12, "write(byte)/readByte(), positive");
      raf.seek(0);
      raf.writeByte(-12);
      raf.seek(0);
      harness.check(raf.readByte(), -12, "writeByte(byte)/readByte(), negative");

      // writeBoolean/readBoolean
      raf.seek(0);
      raf.writeBoolean(true);
      raf.writeBoolean(false);
      raf.seek(0);
      harness.check(raf.readBoolean(), "writeBoolean(T)/readBoolean()");
      harness.check(!raf.readBoolean(), "writeBoolean(F)/readBoolean()");

      // writeShort/readShort
      raf.seek(0);
      raf.writeShort(527);
      raf.seek(0);
      harness.check(raf.readShort(), 527, "writeShort(n)/readShort()");

      // writeUTF/readUTF
      raf.seek(0);
      raf.writeUTF(teststr);
      raf.seek(0);
      harness.check(raf.readShort(), testlength, "writeUTF(s): length encoding");
      raf.seek(0);
      harness.check(raf.readUTF(), teststr,
                    "writeUTF(s)/readUTF: string recovery");

      // writeBytes/readLine
      // N.B.: This test actually tests to the JDK1.2 specification.  JDK1.1 says, in part:
      //   The line-terminating character(s), if any, are included as part of the string returned. 
      // The actual behavior, and spec'd in 1.2, is to strip the line terminator.  Its presence is 
      //   inferred from readLine's returning the correct string up to, but not including, the terminator.
      raf.seek(0);
      raf.writeBytes("foobar\n");
      raf.seek(0);
      harness.check(raf.readLine(), "foobar", "writeBytes(s)/readLine()");

      // writeChar(c)/writeChars(s)/readChar()
      raf.seek(0);
      raf.writeChar('f');
      raf.writeChars("oobar");
      raf.seek(0);

      String s = "";

      for (int i = 0; i < 6; i++)
        s += raf.readChar();

      harness.check(s, "foobar", "writeChar/writeChars/readChar()");

      // writeLong/readLong
      raf.seek(0);
      raf.writeLong(123456L);
      raf.seek(0);
      harness.check(raf.readLong(), 123456L, "writeLong(l)/readLong()");

      // writeFloat/readFloat
      raf.seek(0);
      raf.writeFloat(123.45F);
      raf.seek(0);
      harness.check(raf.readFloat(), 123.45F, "writeFloat(l)/readFloat()");

      // writeDouble/readDouble
      raf.seek(0);
      raf.writeDouble(123.45D);
      raf.seek(0);
      harness.check(raf.readDouble(), 123.45D, "writeDouble(l)/readDouble()");

      // writeInt/readInt
      raf.seek(0);
      raf.writeInt(12345);
      raf.seek(0);
      harness.check(raf.readInt(), 12345, "writeInt(l)/readInt()");

      // readUnsignedByte/readUnsignedShort
      raf.seek(2);
      harness.check(raf.readUnsignedByte(), 48, "readUnsignedByte()");
      raf.seek(2);
      harness.check(raf.readUnsignedShort(), 12345, "readUnsignedShort()");
    }
    catch (IOException e)
    {
      harness.debug(e);
      harness.fail("IOException after opening file");
    }

    // close()
    try
    {
      raf.close();
    }
    catch (IOException e)
    {
      harness.check(false, "close()");
    }
  }
}
