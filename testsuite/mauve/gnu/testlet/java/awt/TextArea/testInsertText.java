/* testInsertText.java  
 Copyright (C) 2006 Tania Bento <tbento@redhat.com>
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

package gnu.testlet.java.awt.TextArea;

import java.awt.TextArea;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class testInsertText implements Testlet
{

  public void test(TestHarness harness)
  {
    TextArea a = new TextArea("World");
    
    // Insert at the beginning of text.
    harness.check(a.getPeer(), null);
    a.insertText("Hello ", 0);
    harness.check(a.getText(), "Hello World");
    
    // Insert at the end of text.
    harness.check(a.getPeer(), null);
    a.insertText("!", a.getText().length());
    harness.check(a.getText(), "Hello World!");
    
    // Insert in the middle of text.
    harness.check(a.getPeer(), null);
    a.insertText(" There", 5);
    harness.check(a.getText(), "Hello There World!");
  }

}
