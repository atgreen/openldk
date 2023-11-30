/*************************************************************************
/* jdk11.java -- java.io.FileReader 1.1 tests
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

package gnu.testlet.java.io.FileReader;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.FileReader;
import java.io.FileInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

public class jdk11 implements Testlet
{
  
  public void test (TestHarness harness)
  {
    String tmpfile = harness.getTempDirectory()
      + File.separator + "mauve-jdk11.tst";
    File f = new File(tmpfile);

    // Make sure the file exists.
    try
      {
	f.createNewFile();
      }
    catch (IOException ioe)
      {
	harness.debug(ioe);
      }

    try {
      FileReader fr1 = new FileReader(tmpfile);
	  harness.check(true, "FileReader(string)");
    }
    catch (FileNotFoundException e) {
      harness.fail("Can't open file " + tmpfile);
    } 

    try {
      File f2 = new File(tmpfile);
      FileReader fr2 = new FileReader(f2);
	  harness.check(true, "FileReader(File)");
	  FileInputStream fis = new FileInputStream(f2);
      try {
	    FileReader fr3 = new FileReader(fis.getFD());
	    harness.check(true, "FileReader(FileDescriptor)");
      }
      catch (IOException e) {
		harness.fail("Couldn't get FileDescriptor)");
      }
    }
    catch (FileNotFoundException e) {
      harness.fail("Can't open file " + tmpfile);
    } 

    // Cleanup
    f.delete();
 }
}


