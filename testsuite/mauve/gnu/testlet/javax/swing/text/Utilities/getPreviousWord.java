/* getPreviousWord.java
   Copyright (C) 2006  Robert Schuster <robertschuster@fsfe.org>
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

// Tags: 1.2

package gnu.testlet.javax.swing.text.Utilities;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JTextField;

import javax.swing.text.Utilities;
import javax.swing.text.BadLocationException;

public class getPreviousWord implements Testlet
{
  String text1 = "GNU Classpath, Essential Libraries for Java, " +
                "is a GNU project to create free core class " +
                "libraries for use with virtual machines and " +
                "compilers for the java programming language.";

  JTextField tf = new JTextField(text1);

  int[] expected1 = new int[] { 176, 175, 167, 155, 150, 146, 142, 132, 128,
                               119, 111, 106, 102, 98, 88, 82, 77, 72, 65, 62,
                               54, 50, 48, 45, 43, 39, 35, 25, 15, 13, 4, 0
                              };

  String text2 = "foo 333 . **777.1)/&";

  int[] expected2 = new int[] { 19, 18, 17, 12, 11, 10, 8, 4, 0 };

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
        // At first Utilities.getNextWord() has to return the correct offsets
        // for the given text.
        harness.checkPoint("indices-normal");
        try
          {
            for ( int i=0; i < expected1.length - 1; i++)
	      harness.check(Utilities.getPreviousWord(tf, expected1[i]), expected1[i+1]);
          }
        catch (BadLocationException ble)
          {
            ble.printStackTrace();
	    harness.verbose("index: "  + ble.offsetRequested());
            harness.fail("BadLocationException occurred!");
          }

        // Given an offset > text.length() the method should throw a
        // BadLocationException.
        harness.checkPoint("exception");
        boolean correctException = false;
        try
          {
	      Utilities.getPreviousWord(tf, 180);
          }
        catch (Exception e)
          {
            correctException = e instanceof BadLocationException;
          }
        harness.check(correctException);

	// Given an offset <= 0 the method should throw a
        // BadLocationException..
	correctException = false;
        try
          {
	      Utilities.getPreviousWord(tf, 0);
          }
        catch (Exception e)
          {
            correctException = e instanceof BadLocationException;
          }
        harness.check(correctException);
        
	correctException = false;
        try
          {
	      Utilities.getPreviousWord(tf, -1);
          }
        catch (Exception e)
          {
            correctException = e instanceof BadLocationException;
          }
        harness.check(correctException);

        // Do the index check again with a more complicated text.
	tf.setText(text2);
        harness.checkPoint("indices-tricky");
        try
          {
            for ( int i=0; i < expected2.length - 1; i++)
	      harness.check(Utilities.getPreviousWord(tf, expected2[i]), expected2[i+1]);
          }
        catch (BadLocationException ble)
          {
            ble.printStackTrace();
	    harness.verbose("index: "  + ble.offsetRequested());
            harness.fail("BadLocationException occurred!");
          }
  }

}
