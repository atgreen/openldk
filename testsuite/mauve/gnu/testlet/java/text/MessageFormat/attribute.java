/* attribute.java -- tests formatToCharacterIterator
   Copyright (C) 2003 Free Software Foundation

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

// TAGS: JDK1.4
package gnu.testlet.java.text.MessageFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;
import java.math.BigInteger;
import java.util.Map;

public class attribute implements Testlet {
  private static class AttrTest {
    Object[] args;
    String expected;
    
    AttrTest(Object[] args, String expected)
    {
      this.args = args;
      this.expected = expected;
    }
  }

  private AttrTest[] tests = new AttrTest[] {
    new AttrTest(new Object[] {
      new Integer(10), "Hello !" }, "10 Hello !"),
    new AttrTest(new Object[] {
      new Float(70.1), "World !" }, "70.1 World !"),
    new AttrTest(new Object[] {
      new Float(70.1), new Integer(40) }, "70.1 40"),
    new AttrTest(new Object[] {
      new Float(70.1), new BigInteger("193289045") }, "70.1 193,289,045")    
  };

  final private void test_Basic(TestHarness harness)
  {
    MessageFormat format;

    try
      {
	format = new MessageFormat("{0,number} {1}");
      }
    catch (Exception e)
      {
	harness.debug(e);
	harness.fail("Unexpected exception " + e);
	return;
      }
    
    harness.checkPoint("null argument");
    try
      {
	format.formatToCharacterIterator(null);
	harness.debug("It should have thrown an exception here");
	harness.check(false);
      }
    catch (NullPointerException _)
      {
	harness.check(true); 
      }
    catch (Exception e)
      {
  	harness.debug(e);
	harness.fail("Unexpected exception " + e);
      }

    harness.checkPoint("Illegal arguments");
    try
      {
	format.formatToCharacterIterator
	  (new Object[] { "Hello world !", "Hello 2" });
	harness.check(false);
      }
    catch (IllegalArgumentException _)
      {
	harness.check(true);
      }
    catch (Exception e)
      {
	harness.debug(e);
	harness.check(false, "unexpected exception");
      }
  }
  
  final private void test_StringBuild(TestHarness harness)
  {
    MessageFormat format;

    try
      {
	format = new MessageFormat("{0,number} {1}");
      }
    catch (Exception e)
      {
	harness.debug(e);
	harness.fail("Unexpected exception " + e);
	return;
      }
    
    harness.checkPoint("Valid arguments");
    for (int i = 0; i < tests.length; i++)
      {
	try
	  {
	    AttributedCharacterIterator iterator =
	      format.formatToCharacterIterator(tests[i].args);
	
	    harness.check(iteratorToString(iterator), tests[i].expected);
	  }
	catch (Exception e)
	  {
	    harness.debug(e);
	    harness.check(false);
	  }
      }
  }

  public void test(TestHarness harness)
  {
    test_Basic(harness);
    test_StringBuild(harness);
    test_Attributes(harness);
    test_FieldPos(harness);
  }

  final private void test_Attributes(TestHarness harness)
  { 
    harness.checkPoint("Attributes");
    try
      {
	String before = "Original unmarked -- ";
	String after = " -- marked";
	String marked = "Hello world !";
	String format_string = before + "{0}" + after;
	MessageFormat format2 = new MessageFormat(format_string);
	AttributedCharacterIterator iterator = 
	  format2.formatToCharacterIterator(new Object[] { marked });
	int[] range = new int[] { before.length(), marked.length() + before.length(),
				  format_string.length() };
	Object[] attrs = new Object[] { null, new Integer(0), null };
	int i, j;
	char c; 
	for (c = iterator.first(), i = 0, j = 0; c != CharacterIterator.DONE; j++, c = iterator.next())
	  {
	    if (range[i] == j)
	      i++;
	    if (attrs[i] != null)
	      {
		Map m = iterator.getAttributes();
		Object o = m.get(MessageFormat.Field.ARGUMENT);
		
		harness.check(o, attrs[i]);
	      }
	    else
	      {
		harness.check(iterator.getAttributes().get(MessageFormat.Field.ARGUMENT), null);
	      }
	  }
      }
    catch (Exception e)
      {
	harness.debug(e);
	harness.check(false);
      }
  }

  final private void test_FieldPos(TestHarness harness)
  {
    harness.checkPoint("Field position");
    try
      {
	MessageFormat format = new MessageFormat("test field {0}");
	FieldPosition pos = new FieldPosition(MessageFormat.Field.ARGUMENT);
	StringBuffer output = new StringBuffer(25);

	format.format(new Object[] { "position" }, output, pos);

	harness.check(output.toString(), "test field position");
	harness.check(pos.getBeginIndex(), 11);
	harness.check(pos.getEndIndex(), 19);
      }
    catch (Exception e)
      {
	harness.debug(e);
	harness.check(false);
      }
  }

  private String iteratorToString(CharacterIterator iterator)
  {
    StringBuffer sb = new StringBuffer(iterator.getEndIndex()-iterator.getBeginIndex());
    
    for(char c = iterator.first(); c != CharacterIterator.DONE; c = iterator.next()) {
      sb.append(c);
    }
    
    return sb.toString();
  }
}
