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

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileSystemView;

/**
 * Some checks for the getFileSystemView() method of the 
 * {@link JFileChooser} class.
 */
public class getFileSystemView implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    JFileChooser jfc = new JFileChooser();
    harness.check(jfc.getFileSystemView(), FileSystemView.getFileSystemView());
    
    FileSystemView fsv1 = new MyFileSystemView();
    jfc.setFileSystemView(fsv1);
    harness.check(jfc.getFileSystemView(), fsv1);
    
    jfc.setFileSystemView(null);
    harness.check(jfc.getFileSystemView(), null);
  }

}
