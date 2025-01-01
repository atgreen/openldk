// Tags: JDK1.5

/*
   Copyright (C) 2005 Andrew John Hughes (gnu_andrew@member.fsf.org)

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

package gnu.testlet.java.lang.Character;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class Blocks15
  implements Testlet
{

  public void test (TestHarness harness)
  {
    harness.check(Character.UnicodeBlock.forName("Greek"), Character.UnicodeBlock.GREEK);
    harness.check(Character.UnicodeBlock.forName("Combining Marks for Symbols"),
		  Character.UnicodeBlock.COMBINING_MARKS_FOR_SYMBOLS);
    harness.check(Character.UnicodeBlock.forName("CombiningMarksforSymbols"),
		  Character.UnicodeBlock.COMBINING_MARKS_FOR_SYMBOLS);
     harness.check(Character.UnicodeBlock.forName("Miscellaneous Mathematical Symbols-B"),
		   Character.UnicodeBlock.MISCELLANEOUS_MATHEMATICAL_SYMBOLS_B);
     harness.check(Character.UnicodeBlock.forName("MiscellaneousMathematicalSymbols-B"),
		   Character.UnicodeBlock.MISCELLANEOUS_MATHEMATICAL_SYMBOLS_B);
     harness.check(Character.UnicodeBlock.forName("Miscellaneous_Mathematical_Symbols_B"),
		   Character.UnicodeBlock.MISCELLANEOUS_MATHEMATICAL_SYMBOLS_B);
     try
       {
	 Character.UnicodeBlock.forName(null);
	 harness.fail("null allowed to forName()");
       }
     catch (NullPointerException e)
       {
	 harness.check(true);
       }
     try
       {
	 Character.UnicodeBlock.forName("GNU Classpath Characters");
	 harness.fail("Allowed request for invalid character set to forName()");
       }
     catch (IllegalArgumentException e)
       {
	 harness.check(true);
       }
     harness.check(Character.UnicodeBlock.of(0x2191), Character.UnicodeBlock.ARROWS);
     harness.check(Character.UnicodeBlock.of(0x100000),
		   Character.UnicodeBlock.SUPPLEMENTARY_PRIVATE_USE_AREA_B);
     try
       {
	 Character.UnicodeBlock.of(0x200000);
	 harness.fail("Allowed invalid codepoint to of(int)");
       }
     catch (IllegalArgumentException e)
       {
	 harness.check(true);
       }
  }

}
