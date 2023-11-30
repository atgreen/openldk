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
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularType;

/**
 * Tests {@link ArrayType(int,javax.management.openmbean.OpenType} constructor.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public class Constructor1
  implements Testlet
{
  
  public void test(TestHarness h)
  {
    try
      {
	ArrayType type = new ArrayType(0, SimpleType.INTEGER);
	h.fail("Didn't catch dimensions < 1");
      }
    catch (IllegalArgumentException e)
      {
	h.check(true, "Threw exception for dimensions of 0");
      }
    catch (OpenDataException e)
      {
	h.debug(e);
      }
    try
      {
	ArrayType type = new ArrayType(-1, SimpleType.INTEGER);
	h.fail("Didn't catch dimensions < 1");
      }
    catch (IllegalArgumentException e)
      {
	h.check(true, "Threw exception for dimensions of -1");
      }
    catch (OpenDataException e)
      {
	h.debug(e);
      }
    try
      {
	h.checkPoint("1-dimensional String array");
	ArrayType type = new ArrayType(1, SimpleType.STRING);
	h.check(type.getClassName(), "[Ljava.lang.String;");
	h.check(type.getTypeName(), "[Ljava.lang.String;");
	h.check(type.getElementOpenType().getClassName(), "java.lang.String");
	h.check(type.getDescription(), "1-dimension array of java.lang.String");
	h.checkPoint("2-dimensional String array");
	ArrayType type2 = new ArrayType(2, SimpleType.STRING);
	h.check(type2.getClassName(), "[[Ljava.lang.String;");
	h.check(type2.getTypeName(), "[[Ljava.lang.String;");
	h.check(type2.getElementOpenType().getClassName(), "java.lang.String");
	h.check(type2.getDescription(), "2-dimension array of java.lang.String");
	h.checkPoint("4-dimensional String array (one constructor)");
	ArrayType type3 = new ArrayType(4, SimpleType.STRING);
	h.check(type3.getClassName(), "[[[[Ljava.lang.String;");
	h.check(type3.getTypeName(), "[[[[Ljava.lang.String;");
	h.check(type3.getElementOpenType().getClassName(), "java.lang.String");
	h.check(type3.getDescription(), "4-dimension array of java.lang.String");
	h.checkPoint("4-dimensional String array (two constructors)");
	ArrayType type4 = new ArrayType(2, type2);
	h.check(type4.getClassName(), "[[[[Ljava.lang.String;");
	h.check(type4.getTypeName(), "[[[[Ljava.lang.String;");
	h.check(type4.getElementOpenType().getClassName(), "java.lang.String");
	h.check(type4.getDescription(), "4-dimension array of java.lang.String");
	h.checkPoint("Composite Type Array");
	CompositeType ctype = new CompositeType("Test","Test",new String[]{"name"},
						new String[]{"Name"},
						new OpenType[] { SimpleType.STRING});
	ArrayType type5 = new ArrayType(1, ctype);
	String className = CompositeData.class.getName();
	h.check(type5.getClassName(), "[L" + className + ";");
	h.check(type5.getTypeName(), "[L" + className + ";");
	h.check(type5.getElementOpenType().getClassName(), className);
	h.check(type5.getDescription(), "1-dimension array of " + className);
	h.checkPoint("Tabular Type Array");
	TabularType ttype = new TabularType("Test","Test",ctype,new String[]{"name"});
	ArrayType type6 = new ArrayType(1, ttype);
	className = TabularData.class.getName();
	h.check(type6.getClassName(), "[L" + className + ";");
	h.check(type6.getTypeName(), "[L" + className + ";");
	h.check(type6.getElementOpenType().getClassName(), className);
	h.check(type6.getDescription(), "1-dimension array of " + className);	
      }
    catch (OpenDataException e)
      {
	h.debug(e);
      }
    try
      {
	ArrayType type = new ArrayType(-1, new OpenType("Mauve","Mauve","Mauve")
	  {
	    public boolean equals(Object obj)
	    {
	      return false;
	    }

	    public int hashCode()
	    {
	      return 42;
	    }

	    public boolean isValue(Object obj)
	    {
	      return false;
	    }

	    public String toString()
	    {
	      return "Mauve";
	    }
	  });
	h.fail("Didn't catch our own OpenType");
      }
    catch (OpenDataException e)
      {
	h.check(true, "Threw exception for invalid OpenType");
      }
    
  }
  
}
