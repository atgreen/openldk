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
public class ContentType implements Testlet {
	
	public void test(TestHarness harness) {
          JEditorPane pane = new JEditorPane();
          harness.checkPoint ("Default EditorKits for content types");
          harness.check 
            (pane.getEditorKitForContentType
             ("text/plain").getClass().getName(), 
             "javax.swing.JEditorPane$PlainEditorKit");
          harness.check 
            (JEditorPane.getEditorKitClassNameForContentType("text/plain"), 
             "javax.swing.JEditorPane$PlainEditorKit");

          harness.check 
            (pane.getEditorKitForContentType
             ("text/html").getClass(), HTMLEditorKit.class);
          harness.check 
            (pane.getEditorKitForContentType
             ("text/rtf").getClass(), RTFEditorKit.class);
          harness.check 
            (pane.getEditorKitForContentType
             ("application/rtf").getClass(), RTFEditorKit.class);

          
          harness.checkPoint ("Registering an EditorKit for a content type");
          harness.check 
            (pane.getEditorKitForContentType
             ("foobar").getClass().getName(), 
             "javax.swing.JEditorPane$PlainEditorKit");
          harness.check (JEditorPane.getEditorKitClassNameForContentType("foobar") == null);
          harness.check 
            (JEditorPane.createEditorKitForContentType("foobar") == null);
          JEditorPane.registerEditorKitForContentType
            ("foobar", "javax.swing.text.html.HTMLEditorKit");
          harness.check 
            (pane.getEditorKitForContentType("foobar").getClass(), 
             HTMLEditorKit.class);   
          harness.check 
            (JEditorPane.createEditorKitForContentType("foobar").getClass(), 
             HTMLEditorKit.class);

          // Explicitly setting the EditorKit takes precedence over the
          // registered type
          pane.setEditorKitForContentType ("foobar", new RTFEditorKit());
          harness.check 
            (pane.getEditorKitForContentType("foobar").getClass(), 
             RTFEditorKit.class);   

          // We can set the EditorKit for content types previously unintroduced
          pane.setEditorKitForContentType ("tony", new RTFEditorKit());
          harness.check 
            (pane.getEditorKitForContentType("tony").getClass(), 
             RTFEditorKit.class);   
          // But this only affects the instance of JEditorPane, not the class itself
          harness.check (JEditorPane.createEditorKitForContentType("tony") == null);
     
	}

}
