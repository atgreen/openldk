// Tags: not-a-test 

// Copyright (C) 2005 Audrius Meskauskas <audriusa@bluewin.ch>

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.gnu.javax.swing.text.html.parser.support.Parser;

import gnu.testlet.TestHarness;

/**
 * A helper class, not a test.
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class TestCase
{
  public TestHarness h;

  public TestCase()
  {
    try
      {
        setUp();
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
      }
  }

  public void assertEquals(String msg, Object a, Object b)
  {
    if (a==b) return;
    h.check(a, b, msg);
  }

  public void assertEquals(Object a, Object b)
  { 
    if (a==b) return;
    h.check(a, b);
  }

  public void assertEquals(int a, int b)
  {
    h.check(a, b);
  }

  public void assertEquals(String msg, int a, int b)
  {
    h.check(a, b, msg);
  }

  public void assertEquals(boolean a, boolean b)
  {
    h.check(a, b);
  }

  public void assertFalse(String msg, boolean a)
  {
    h.check(!a, msg);
  }

  public void assertFalse(boolean a)
  {
    h.check(!a, "Must be false");
  }

  public void assertNotNull(String msg, Object a)
  {
    h.check(a != null, "Must be not null");
  }

  public void assertNull(String msg, Object a)
  {
    h.check(a == null, "Must be null");
  }

  public void assertTrue(String msg, boolean a)
  {
    h.check(a, msg);
  }

  public void assertTrue(boolean a)
  {
    h.check(a, "must be True");
  }

  protected void setUp()
                throws Exception
  {
  }

  protected void tearDown()
                   throws Exception
  {
  }
}
