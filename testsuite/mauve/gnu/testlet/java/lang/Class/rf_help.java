// A helper for reflect.java.
// Tags: not-a-test

// Copyright (C) 2000 Cygnus Solutions

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.lang.Class;

public class rf_help
{

  public static class inner {
  }

  private int size;
  public String name;
  static double value = 1.0;

  static void doit()
  {
    value = 1.0;
  }

  private rf_help()
  {
    this(0.0);
  }

  public rf_help(double arg) 
  {
    size = 0;
    name = "";
  }
}
