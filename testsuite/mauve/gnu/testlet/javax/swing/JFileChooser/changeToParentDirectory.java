// Tags: JDK1.2

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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.List;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileSystemView;

/**
 * Some checks for the changeToParentDirectory() method of the 
 * {@link JFileChooser} class.
 */
public class changeToParentDirectory 
  implements Testlet, PropertyChangeListener 
{

  List events = new java.util.ArrayList();;
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    // this test shows an unusual behaviour of this method - if the current
    // directory is a root directory, changing to the parent directory
    // makes the current directory the user's home directory...
    JFileChooser fc = new JFileChooser();
    FileSystemView fsv = fc.getFileSystemView();
    File[] roots = fsv.getRoots();
    File root = roots[0];
    fc.setCurrentDirectory(root);
    fc.addPropertyChangeListener(this);
    fc.changeToParentDirectory();
    harness.check(fc.getCurrentDirectory(), 
            new File(System.getProperty("user.home")));
    harness.check(events.size(), 1);
  }

}
