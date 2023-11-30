//Tags: JDK1.4

//Copyright (C) 2005 Free Software Foundation, Inc.
//Written by Wolfgang Baer (WBaer@gmx.de)

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
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */


package gnu.testlet.javax.print.attribute.AttributeSetUtilities;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.print.attribute.Attribute;
import javax.print.attribute.AttributeSet;
import javax.print.attribute.AttributeSetUtilities;

/**
 * Simple test if correctly NPEs, ClassCastExceptions
 * and IllegalArgumentExceptions are thrown.
 */
public class simple implements Testlet
{

  /**
   * A simple attribute implementation.
   */
  class SimpleAttribute implements Attribute
  {
    private int value;

    public SimpleAttribute(int value)
    {
      this.value = value;
    }
    public Class getCategory()
    {
      return this.getClass();
    }
    public String getName()
    {
      return "SimpleAttribute";
    }
    public boolean equals(Object obj)
    {
      if (obj instanceof SimpleAttribute)
        {
          SimpleAttribute att = (SimpleAttribute) obj;
          if (att.value == this.value)
            return true;
        }
      return false;
    }
    public int hashCode()
    {
      return this.value;
    }
  }

  public void test(TestHarness harness)
  {
    // must throw NPE
    harness.checkPoint("NPE tests");
    try
      {
        AttributeSetUtilities.synchronizedView((AttributeSet) null);
        harness.check(false);
      }
    catch (NullPointerException e)
      {
        harness.check(true);
      }

    // must throw NPE
    try
      {
        AttributeSetUtilities.unmodifiableView((AttributeSet) null);
        harness.check(false);
      }
    catch (NullPointerException e)
      {
        harness.check(true);
      }

    harness.checkPoint("casting tests");
    try
      {
        AttributeSetUtilities.verifyAttributeCategory(SimpleAttribute.class,
                                                      Attribute.class);
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.check(false);
      }
    try
      {
        AttributeSetUtilities.verifyAttributeCategory(String.class,
                                                      Attribute.class);
        harness.check(false);
      }
    catch (ClassCastException e)
      {
        harness.check(true);
      }

    try
      {
        AttributeSetUtilities.verifyAttributeValue(new SimpleAttribute(1),
                                                   Attribute.class);
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.check(false);
      }
    try
      {
        AttributeSetUtilities.verifyAttributeValue(new String(),
                                                   Attribute.class);
        harness.check(false);
      }
    catch (ClassCastException e)
      {
        harness.check(true);
      }

    try
      {
        AttributeSetUtilities.verifyCategoryForValue(SimpleAttribute.class,
                                                     new SimpleAttribute(1));
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.check(false);
      }

    try
      {
        AttributeSetUtilities.verifyCategoryForValue(String.class, 
                                                     new SimpleAttribute(1));
        harness.check(false);
      }
    catch (IllegalArgumentException e)
      {
        harness.check(true);
      }
  }

}
