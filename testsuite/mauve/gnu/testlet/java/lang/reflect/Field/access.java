// Tags: JDK1.1

// Copyright (C) 2003 Red Hat, Inc.

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

package gnu.testlet.java.lang.reflect.Field;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.lang.reflect.Field;

public class access implements Testlet
{
  int value;

  public void test(TestHarness harness)
  {
    // Regression test for libgcj bug.
    // See http://gcc.gnu.org/bugzilla/show_bug.cgi?id=11779
    try
      {
	Field field = access.class.getDeclaredField("value");
	field.setInt(this, 777);
      }
    catch (Exception ignore)
      {
      }
    harness.check(value, 777);
  }
}
