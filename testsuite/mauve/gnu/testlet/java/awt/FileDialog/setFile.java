/* testSetFile.java 
   Copyright (C) 2006 Red Hat
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: 1.4

package gnu.testlet.java.awt.FileDialog;

import java.awt.FileDialog;
import java.awt.Frame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class setFile implements Testlet 
{

  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
    test3(harness);
  }
  
  public void test1(TestHarness harness)
  {
    FileDialog fd = new FileDialog(new Frame());
    fd.setFile("String");
    harness.check(fd.getFile(), "String");
  }
  
  public void test2(TestHarness harness)
  {
    FileDialog fd = new FileDialog(new Frame());
    fd.setFile(null);
    harness.check(fd.getFile(), null);
  }
  
  public void test3(TestHarness harness)
  {
    FileDialog fd = new FileDialog(new Frame());
    fd.setFile("");
    harness.check(fd.getFile(), null);
  }

}
