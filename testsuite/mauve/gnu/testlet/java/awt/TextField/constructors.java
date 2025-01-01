/* constructors.java -- some checks for the constructors in the TextField
       class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

// Tags: JDK1.0

package gnu.testlet.java.awt.TextField;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.TextField;

public class constructors implements Testlet
{
  public void test(TestHarness harness)
  {
      testConstructor1(harness);
      testConstructor2(harness);
      testConstructor3(harness);
      testConstructor4(harness);
  }
  
  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("()");
    TextField tf = new TextField();
    harness.check(tf.getText(), "");
    harness.check(tf.isEditable());
    harness.check(tf.getColumns(), 0);
    harness.check(tf.getEchoChar(), 0);
  }
  
  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(int)");
    TextField tf = new TextField(3);
    harness.check(tf.getText(), "");
    harness.check(tf.isEditable());
    harness.check(tf.getColumns(), 3);
    harness.check(tf.getEchoChar(), 0);
    
    // try negative columns
    tf = new TextField(-1);
    harness.check(tf.getText(), "");
    harness.check(tf.isEditable());
    harness.check(tf.getColumns(), 0);
    harness.check(tf.getEchoChar(), 0);
  }
  
  public void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("(String)");
    TextField tf = new TextField("ABC");
    harness.check(tf.getText(), "ABC");
    harness.check(tf.isEditable());
    harness.check(tf.getColumns(), 3);
    harness.check(tf.getEchoChar(), 0);
    
    // try null
    tf = new TextField(null);
    harness.check(tf.getText(), "");
    harness.check(tf.isEditable());
    harness.check(tf.getColumns(), 0);
    harness.check(tf.getEchoChar(), 0);
  }
  
  public void testConstructor4(TestHarness harness)
  {
    harness.checkPoint("(String, int)");
    TextField tf = new TextField("ABC", 3);
    harness.check(tf.getText(), "ABC");
    harness.check(tf.isEditable());
    harness.check(tf.getColumns(), 3);
    harness.check(tf.getEchoChar(), 0);
    
    // try null
    tf = new TextField(null, 3);
    harness.check(tf.getText(), "");
    harness.check(tf.isEditable());
    harness.check(tf.getColumns(), 3);
    harness.check(tf.getEchoChar(), 0);
    
    // try negative columns
    tf = new TextField("ABC", -3);
    harness.check(tf.getText(), "ABC");
    harness.check(tf.isEditable());
    harness.check(tf.getColumns(), 0);
    harness.check(tf.getEchoChar(), 0);
  }
}
