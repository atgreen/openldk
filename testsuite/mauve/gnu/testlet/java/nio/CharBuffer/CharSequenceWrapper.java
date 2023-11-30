/* CharSequenceWrapper.java -- Tests the CharSequence wrapping CharBuffer
   Copyright (C) 2007 Roman Kennke (kennke@aicas.com)
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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.4

package gnu.testlet.java.nio.CharBuffer;

import java.nio.CharBuffer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * This tests the CharBuffer that wraps a CharSequence.
 *
 * @author Roman Kennke (kennke@aicas.com)
 *
 */
public class CharSequenceWrapper implements Testlet
{

  public void test(TestHarness harness)
  {
    testSlice(harness);
    testDuplicate(harness);
    testBasic(harness);
  }

  /**
   * Tests some basic properties of a char sequence wrapping char buffer.
   *
   * @param h the test harness
   */
  private void testBasic(TestHarness h)
  {
    h.checkPoint("testBasic");
    StringBuilder b = new StringBuilder("Hello World");
    CharBuffer cb = CharBuffer.wrap(b, 4, 7);
    try
      {
        cb.arrayOffset();
        h.fail("testBasic");
      }
    catch (UnsupportedOperationException ex)
      {
        h.check(true);
      }
    try
      {
        cb.array();
        h.fail("testBasic");
      }
    catch (UnsupportedOperationException ex)
      {
        h.check(true);
      }
    h.check(cb.capacity(), b.length());
    h.check(cb.hasArray(), false);
    h.check(cb.hasRemaining(), true);
    h.check(cb.isDirect(), false);
    h.check(cb.isReadOnly(), true);
    h.check(cb.length(), 3);
    h.check(cb.limit(), 7);
    h.check(cb.position(), 4);
    h.check(cb.remaining(), 3);
    try
      {
        cb.compact();
        h.fail("testBasic");
      }
    catch (UnsupportedOperationException ex)
      {
        h.check(true);
      }
  }

  /**
   * Tests how slicing affects the wrapped char buffer.
   *
   * @param h the test harness
   */
  private void testSlice(TestHarness h)
  {
    StringBuilder b = new StringBuilder("Hello World");
    CharBuffer cb = CharBuffer.wrap(b);
    cb.position(4);
    cb.limit(7);
    CharBuffer slice = cb.slice();

    h.check(slice.capacity(), 3);
    try
      {
        slice.arrayOffset();
        h.fail("testSlice");
      }
    catch (UnsupportedOperationException ex)
      {
        h.check(true);
      }
    h.check(slice.hasArray(), false);
    try
      {
        slice.array();
        h.fail("testSlice");
      }
    catch (UnsupportedOperationException ex)
      {
        h.check(true);
      }
    h.check(slice.isDirect(), false);
    h.check(slice.isReadOnly(), true);
    h.check(slice.length(), 3);
    h.check(slice.limit(), 3);
    h.check(slice.position(), 0);

    // This shows a JDK bug..
    h.check(slice.get(), 'o');
    h.check(slice.get(), ' ');
    h.check(slice.get(), 'W');

  }

  /**
   * Tests how duplicating affects the wrapped char buffer.
   *
   * @param h the test harness
   */
  private void testDuplicate(TestHarness h)
  {
    StringBuilder b = new StringBuilder("Hello World");
    CharBuffer cb = CharBuffer.wrap(b);
    cb.position(4);
    cb.limit(7);
    CharBuffer dup = cb.duplicate();

    h.check(dup.capacity(), 11);
    try
      {
        dup.arrayOffset();
        h.fail("testDuplicate");
      }
    catch (UnsupportedOperationException ex)
      {
        h.check(true);
      }
    h.check(dup.hasArray(), false);
    try
      {
        dup.array();
        h.fail("testSlice");
      }
    catch (UnsupportedOperationException ex)
      {
        h.check(true);
      }
    h.check(dup.isDirect(), false);
    h.check(dup.isReadOnly(), true);
    h.check(dup.length(), 3);
    h.check(dup.limit(), 7);
    h.check(dup.position(), 4);

    h.check(dup.get(), 'o');
    h.check(dup.get(), ' ');
    h.check(dup.get(), 'W');

  }

}
