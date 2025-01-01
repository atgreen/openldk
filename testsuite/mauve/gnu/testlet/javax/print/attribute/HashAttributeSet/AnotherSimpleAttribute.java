//Tags: not-a-test

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

package gnu.testlet.javax.print.attribute.HashAttributeSet;

import javax.print.attribute.Attribute;

/**
 * Another simple attribute implementation.
 */
public class AnotherSimpleAttribute implements Attribute
{
  private int value;
  
  public AnotherSimpleAttribute(int value) {
    this.value = value;
  }
  
  public Class getCategory()
  {
    return this.getClass();
  }

  public String getName()
  {
    return "AnotherSimpleAttribute";
  }

  public boolean equals(Object obj)
  {
    if (obj instanceof AnotherSimpleAttribute)
      {
        AnotherSimpleAttribute att = (AnotherSimpleAttribute) obj;
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
