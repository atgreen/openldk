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


package gnu.testlet.javax.print.attribute.EnumSyntax;

import javax.print.attribute.EnumSyntax;

/**
 * Helper class testing EnumSyntax implementation which
 * is correctly subclassed according to specification.
 */
public class CorrectEnumSyntax extends EnumSyntax
{
  public static final CorrectEnumSyntax TEST1 = new CorrectEnumSyntax(3);
  public static final CorrectEnumSyntax TEST2 = new CorrectEnumSyntax(4);
  public static final CorrectEnumSyntax TEST3 = new CorrectEnumSyntax(5);

  protected CorrectEnumSyntax(int value)
  {
    super(value);
  }

  protected int getOffset()
  {
    return 3;
  }

  private static final String[] stringTable = { "test1", "test2", "test3"};

  protected String[] getStringTable()
  {
    return stringTable;
  }

  private static final CorrectEnumSyntax[] enumValueTable = 
                                        { TEST1, TEST2, TEST3};

  protected EnumSyntax[] getEnumValueTable()
  {
    return enumValueTable;
  }
}