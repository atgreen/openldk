/*************************************************************************
/* jdk11.java -- java.io.FileWriter 1.1 tests
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

package gnu.testlet.java.io.FileWriter;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.FileWriter;
import java.io.FileOutputStream;
import java.io.File;
import java.io.IOException;

public class jdk11 implements Testlet
{
  
  public void test (TestHarness harness)
  {
    try {
      FileWriter fr1 = new FileWriter("tmpfile");
	  harness.check(true, "FileWriter(string)");
      FileWriter fr1a = new FileWriter("tmpfile", true);
	  harness.check(true, "FileWriter(string, boolean)");
	  File f2 = new File("tmpfile");
      FileWriter fr2 = new FileWriter(f2);
	  harness.check(true, "FileWriter(File)");
	  FileOutputStream fis = new FileOutputStream(f2);
	  FileWriter fr3 = new FileWriter(fis.getFD());
	  harness.check(true, "FileWriter(FileDescriptor)");
    }
    catch (IOException e) {
      harness.fail("Can't open file 'choices'");
    } 
 }
}
