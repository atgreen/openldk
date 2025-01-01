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

public class getWordStart implements Testlet
{
  String text = "GNU Classpath, Essential Libraries for Java, " +
                "is a GNU project to create free core class " +
                "libraries for use with virtual machines and " +
                "compilers for the java programming language.";

  JTextField tf = new JTextField(text);

  int[] expected = new int[] { 0, 0, 0, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 13, 14,
                               15, 15, 15, 15, 15, 15, 15, 15, 15, 24, 25, 25,
                               25, 25, 25, 25, 25, 25, 25, 34, 35, 35, 35, 38,
                               39, 39, 39, 39, 43, 44, 45, 45, 47, 48, 49, 50,
                               50, 50, 53, 54, 54, 54, 54, 54, 54, 54, 61, 62,
                               62, 64, 65, 65, 65, 65, 65, 65, 71, 72, 72, 72,
                               72, 76, 77, 77, 77, 77, 81, 82, 82, 82, 82, 82,
                               87, 88, 88, 88, 88, 88, 88, 88, 88, 88, 97, 98,
                               98, 98, 101, 102, 102, 102, 105, 106, 106, 106,
                               106, 110, 111, 111, 111, 111, 111, 111, 111,
                               118, 119, 119, 119, 119, 119, 119, 119, 119,
                               127, 128, 128, 128, 131, 132, 132, 132, 132,
                               132, 132, 132, 132, 132, 141, 142, 142, 142,
                               145, 146, 146, 146, 149, 150, 150, 150, 150,
                               154, 155, 155, 155, 155, 155, 155, 155, 155,
                               155, 155, 155, 166, 167, 167, 167, 167, 167,
                               167, 167, 167, 175, 175
                             };

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
        // At first Utilities.getNextWord() has to return the correct offsets
        // for the given text.
        harness.checkPoint("indices");
        try
          {
            for ( int i=0; i <= text.length(); i++)
	      harness.check(Utilities.getWordStart(tf, i), expected[i]);
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
  }

}
