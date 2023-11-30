/* Copyright (C) 1999, 2002 Hewlett-Packard Company

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

// Tags: JDK1.1

package gnu.testlet.java.lang.Double;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class DoubleSetterTest implements Testlet
{

  protected static TestHarness harness;
  
  /**
   * Tests the conversion of behaviour of max values when converting from double into Double
   */
  public void test_max()
  {
	// Check directly the MAX_VALUE against NaN 
	harness.check(!Double.isNaN(Double.MAX_VALUE));
	harness.check(!Double.isNaN(new Double(Double.MAX_VALUE).doubleValue()));
	
	// Check the MAX_VALUE against NaN via a direct method setter
	DoubleHolder doubleHolder = new DoubleHolder();
	doubleHolder.setValue(Double.MAX_VALUE);
	harness.check(Double.MAX_VALUE, doubleHolder.getValue());

	// Check the MAX_VALUE against NaN via a setter called by reflection
	DoubleHolder doubleHolder2 = new DoubleHolder();	
	try
	{
	  Method setMethod = DoubleHolder.class.getDeclaredMethod("setValue", new Class[] {double.class});
	  setMethod.invoke(doubleHolder2, new Object[] {new Double(Double.MAX_VALUE)});
	} catch (NoSuchMethodException e) {
	  harness.fail("no method setValue");
	} catch (IllegalAccessException e) {
	  harness.fail("illegal access");
	} catch (InvocationTargetException e) {
	  harness.fail("invocation failed");
	}
	
	harness.check(!Double.isNaN(doubleHolder2.getValue()));
	
  }


  /**
   * Simple holder used to test various way of setting and getting primitive double
   */
  private static class DoubleHolder
  {
    private double value;

	public double getValue()
	{
	  return value;
	}

	public void setValue(double value)
	{
	  this.value = value;
	}
  
  }

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
	test_max();
  }

}
