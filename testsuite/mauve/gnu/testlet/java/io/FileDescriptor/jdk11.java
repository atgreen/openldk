/*************************************************************************
/* jdk11.java -- java.io.FileDescriptor 1.1 tests
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

package gnu.testlet.java.io.FileDescriptor;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.SyncFailedException;

public class jdk11 implements Testlet
{
  
  public void test (TestHarness harness)
  {
   try {
      FileOutputStream fos = new FileOutputStream("tmpfile");
      try {
        FileDescriptor fd = fos.getFD();
        harness.check(fd.valid(), "valid()");
	    try {
			fd.sync();
			harness.check(true, "sync()");
		}
		catch (SyncFailedException e) {
			harness.debug(e);
			harness.fail("SyncFailedException thrown");
        }
      }
      catch (IOException e) {
	    harness.fail("Can't get FileDescriptor");
      }
    }
    catch (FileNotFoundException e) {
      harness.fail("Can't make file 'tmpfile'");
    } 
  }
}


