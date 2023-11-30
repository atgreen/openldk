// Tags: JDK1.5

// Copyright (C) 2007 Andrew John Hughes <gnu_andrew@member.fsf.org>

// This file is part of Mauve.

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.management.openmbean.ArrayType;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.management.openmbean.ArrayType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.SimpleType;

/**
 * Tests {@link ArrayType} constructor
 * for 1-dimensional simple arrays.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public class Constructor2
  implements Testlet
{
  
  public void test(TestHarness h)
  {
    try
      {
	ArrayType type = new ArrayType(SimpleType.STRING, true);
	h.fail("Non-primitive type allowed.");
      }
    catch (OpenDataException e)
      {
	h.check(true, "Exception thrown for primitive array with non-primitive type");
      }
    try
      {
	h.checkPoint("Primitive integer array");
	ArrayType type = new ArrayType(SimpleType.INTEGER, true);
	h.check(type.getClassName(), "[I");
	h.check(type.getTypeName(), "[I");
	h.check(type.getElementOpenType().getClassName(), "java.lang.Integer");
	h.check(type.getDescription(), "1-dimension array of int");
      }
    catch (OpenDataException e)
      {
	h.debug(e);
      }
    try
      {
	h.checkPoint("String array");
	ArrayType type = new ArrayType(SimpleType.STRING, false);
	h.check(type.getClassName(), "[Ljava.lang.String;");
	h.check(type.getTypeName(), "[Ljava.lang.String;");
	h.check(type.getElementOpenType().getClassName(), "java.lang.String");
	h.check(type.getDescription(), "1-dimension array of java.lang.String");
      }
    catch (OpenDataException e)
      {
	h.debug(e);
      }
    
  }
    
  
}
