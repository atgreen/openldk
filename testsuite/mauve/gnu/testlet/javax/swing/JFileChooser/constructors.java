// Tags: JDK1.2
// Uses: MyFileSystemView

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.JFileChooser;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileSystemView;

/**
 * Some checks for the constructors of the {@link JFileChooser} class.
 */
public class constructors implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
    testConstructor6(harness);
  }
  
  public void testConstructor1(TestHarness harness)       
  {
    harness.checkPoint("()");
    JFileChooser fc = new JFileChooser();
    harness.check(fc.getCurrentDirectory(), 
            new File(System.getProperty("user.home")));
  }

  public void testConstructor2(TestHarness harness)       
  {
    harness.checkPoint("(File)");
    File f = new File(System.getProperty("user.home"));
    JFileChooser fc = new JFileChooser(f);
    harness.check(fc.getCurrentDirectory(), f);
    harness.check(fc.getSelectedFile(), null);
    
    // null is OK and defaults to home directory
    fc = new JFileChooser((File) null);
    harness.check(fc.getCurrentDirectory(), f);
    harness.check(fc.getSelectedFile(), null);    
  }

  public void testConstructor3(TestHarness harness)       
  {
    harness.checkPoint("(File, FileSystemView)");
    FileSystemView fsv = new MyFileSystemView();
    File f = new File(System.getProperty("user.home"));
    JFileChooser fc = new JFileChooser(f, fsv);
    harness.check(fc.getCurrentDirectory(), f);
    harness.check(fc.getSelectedFile(), null);
    harness.check(fc.getFileSystemView(), fsv);
    
    // null file is OK and defaults to home directory
    fc = new JFileChooser((File) null, fsv);
    harness.check(fc.getCurrentDirectory(), f);
    harness.check(fc.getSelectedFile(), null);    
    harness.check(fc.getFileSystemView(), fsv);
    
    // null FileSystemView reverts to default
    fc = new JFileChooser(f, null);
    harness.check(fc.getCurrentDirectory(), f);
    harness.check(fc.getSelectedFile(), null);    
    harness.check(fc.getFileSystemView(), FileSystemView.getFileSystemView());    
  }

  public void testConstructor4(TestHarness harness)       
  {
    harness.checkPoint("(FileSystemView)");
    FileSystemView fsv = new MyFileSystemView();
    File f = new File(System.getProperty("user.home"));
    JFileChooser fc = new JFileChooser(fsv);
    harness.check(fc.getCurrentDirectory(), f);
    harness.check(fc.getSelectedFile(), null);
    harness.check(fc.getFileSystemView(), fsv);
    
    // null FileSystemView reverts to default
    fc = new JFileChooser((FileSystemView) null);
    harness.check(fc.getCurrentDirectory(), f);
    harness.check(fc.getSelectedFile(), null);    
    harness.check(fc.getFileSystemView(), FileSystemView.getFileSystemView());    
  }

  public void testConstructor5(TestHarness harness)       
  {
    harness.checkPoint("(String)");
    File f = new File(System.getProperty("user.home"));
    JFileChooser fc = new JFileChooser(System.getProperty("user.home"));
    harness.check(fc.getCurrentDirectory(), f);
    harness.check(fc.getSelectedFile(), null);
    
    // null is OK and defaults to home directory
    fc = new JFileChooser((String) null);
    harness.check(fc.getCurrentDirectory(), f);
    harness.check(fc.getSelectedFile(), null);    
  }

  public void testConstructor6(TestHarness harness)       
  {
    harness.checkPoint("(String, FileSystemView)");
    FileSystemView fsv = new MyFileSystemView();
    File f = new File(System.getProperty("user.home"));
    JFileChooser fc = new JFileChooser(System.getProperty("user.home"), fsv);
    harness.check(fc.getCurrentDirectory(), f);
    harness.check(fc.getSelectedFile(), null);
    harness.check(fc.getFileSystemView(), fsv);
    
    // null file is OK and defaults to home directory
    fc = new JFileChooser((String) null, fsv);
    harness.check(fc.getCurrentDirectory(), f);
    harness.check(fc.getSelectedFile(), null);    
    harness.check(fc.getFileSystemView(), fsv);
    
    // null FileSystemView reverts to default
    fc = new JFileChooser(System.getProperty("user.home"), null);
    harness.check(fc.getCurrentDirectory(), f);
    harness.check(fc.getSelectedFile(), null);    
    harness.check(fc.getFileSystemView(), FileSystemView.getFileSystemView());    
  }

}
