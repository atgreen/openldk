//Tags: JDK1.2

//Copyright (C) 2005 Red Hat

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
//Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.JEditorPane;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import javax.swing.JEditorPane;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.rtf.RTFEditorKit;

/**
 * Tests whether a newly created JEditorPane has the proper EditorKits
 * set up for different content types.
 */ 
public class ConstructorsAndTypes implements Testlet 
{
  
  public void test(TestHarness harness)
  {
    JEditorPane pane = new JEditorPane();
    harness.check 
      (pane.getEditorKit().getClass().getName(), 
       "javax.swing.JEditorPane$PlainEditorKit");
    
    try
      {
        pane = new JEditorPane("http://www.gnu.org");
        harness.check 
          (pane.getEditorKit().getClass(), HTMLEditorKit.class);
      }
    catch (Exception e)
      {
        harness.debug(e);
      }
    
    pane = new JEditorPane ("text/rtf", "hello");
    harness.check 
      (pane.getEditorKit().getClass(), RTFEditorKit.class);
    
    pane = new JEditorPane ("application/rtf", "hello");
    harness.check 
      (pane.getEditorKit().getClass(), RTFEditorKit.class);    
  }  
}
