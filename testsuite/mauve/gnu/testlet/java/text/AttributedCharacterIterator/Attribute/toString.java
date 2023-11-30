/* toString.java -- some checks for the toString() method.
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

// Tags: 1.2

package gnu.testlet.java.text.AttributedCharacterIterator.Attribute;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.AttributedCharacterIterator;

public class toString implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.check(AttributedCharacterIterator.Attribute.INPUT_METHOD_SEGMENT.toString(),
        "java.text.AttributedCharacterIterator$Attribute(input_method_segment)");
    harness.check(AttributedCharacterIterator.Attribute.LANGUAGE.toString(),
        "java.text.AttributedCharacterIterator$Attribute(language)");
    harness.check(AttributedCharacterIterator.Attribute.READING.toString(),
        "java.text.AttributedCharacterIterator$Attribute(reading)");
  }
}
