// Copyright (C) 2004 Max Gilead <gilead@yellowhedgehog.com>

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

package gnu.testlet.java.nio.Buffer;

import gnu.testlet.TestHarness;

import java.nio.Buffer;
import java.nio.InvalidMarkException;

public class WrappedWithOffsetBufferTest
{
  public void test(TestHarness h, BufferFactory factory)
  {
    try
      {
      intialState(h, factory);
      position(h, factory);
      mark(h, factory);
      limit(h, factory);
      rewind(h, factory);
      clear(h, factory);
      flip(h, factory);
      }
    catch(Exception e)
      {
      h.fail("Unexpected excetpion: "+ e);
      h.debug(e);
      }
  }

  private void intialState(TestHarness h, BufferFactory factory)
  {
    Buffer buf = null;

    buf = factory.newInstance();
    checkStatus(h, buf, "intialState", 20, 20, true, 10, 10);
  }
  private void position(TestHarness h, BufferFactory factory)
  {
    Buffer buf = null;

    buf = factory.newInstance();
    h.check(buf.position(1), buf,  "position: buf.position(1)");
    checkStatus(h, buf, "position", 20, 20, true, 19, 1);
    buf.position(20);
    checkStatus(h, buf, "position", 20, 20, false, 0, 20);

    // position can't be negative
    buf = factory.newInstance();
    try
      {
        buf.position(-1);
        h.check(false, "position: is negative");
      }
      catch(IllegalArgumentException iae)
      {
        h.check(true, "position: can't be negative");
      }

    // position can't be larger than limit
    buf = factory.newInstance();
    buf.limit(5);
    try
      {
        buf.position(6);
        h.check(false, "position: is larger than capacity");
      }
      catch(IllegalArgumentException iae)
      {
        h.check(true, "position: can't be larger than capacity");
      }
  }
  private void mark(TestHarness h, BufferFactory factory)
  {
    Buffer buf = null;

    // mark at default position
    buf = factory.newInstance();
    h.check(buf.mark(), buf, "mark: buf.mark()");
    checkStatus(h, buf, "mark", 20, 20, true, 10, 10);
    buf.position(15);
    checkStatus(h, buf, "mark", 20, 20, true, 5, 15);
    h.check(buf.reset(), buf, "mark: buf.reset()");
    checkStatus(h, buf, "mark", 20, 20, true, 10, 10);
    buf.position(16);
    checkStatus(h, buf, "mark", 20, 20, true, 4, 16);
    buf.reset();
    checkStatus(h, buf, "mark", 20, 20, true, 10, 10);

    // mark at specified position
    buf = factory.newInstance();
    buf.position(5);
    buf.mark();
    checkStatus(h, buf, "mark", 20, 20, true, 15, 5);
    buf.position(6);
    checkStatus(h, buf, "mark", 20, 20, true, 14, 6);
    buf.reset();
    checkStatus(h, buf, "mark", 20, 20, true, 15, 5);
    buf.position(7);
    checkStatus(h, buf, "mark", 20, 20, true, 13, 7);
    buf.reset();
    checkStatus(h, buf, "mark", 20, 20, true, 15, 5);

    // mark should be discarded if new position is smaller than mark
    buf = factory.newInstance();
    buf.position(5);
    buf.mark();
    buf.position(4);
    try
      {
        buf.reset();
        h.check(false, "mark: mark not invalidated");
      }
      catch(InvalidMarkException ime)
      {
        h.check(true, "mark: invalidated mark");
      }
    checkStatus(h, buf, "mark", 20, 20, true, 16, 4);
  }
  private void limit(TestHarness h, BufferFactory factory)
  {
    Buffer buf = null;

    buf = factory.newInstance();
    buf.position(2);
    buf.mark();
    buf.position(3);
    h.check(buf.limit(4), buf, "limit: buf.limit(4)");
    checkStatus(h, buf, "limit", 20, 4, true, 1, 3);
    buf.reset();
    checkStatus(h, buf, "limit", 20, 4, true, 2, 2);

    // mark should be discarded if new limit is smaller than mark
    // and position should be set to new limit
    buf = factory.newInstance();
    buf.position(5);
    buf.mark();
    buf.position(6);
    buf.limit(4);
    checkStatus(h, buf, "limit", 20, 4, false, 0, 4);
    try
      {
        buf.reset();
        h.check(false, "limit: mark not invalidated");
      }
      catch(InvalidMarkException ime)
      {
        h.check(true, "limit: invalidated mark");
      }

    // limit can't be negative
    buf = factory.newInstance();
    try
      {
        buf.limit(-1);
        h.check(false, "limit: is negative");
      }
      catch(IllegalArgumentException iae)
      {
        h.check(true, "limit: can't be negative");
      }

    // limit can't be larger than capacity
    buf = factory.newInstance();
    try
      {
        buf.limit(21);
        h.check(false, "limit: is larger than capacity");
      }
      catch(IllegalArgumentException iae)
      {
        h.check(true, "limit: can't be larger than capacity");
      }
  }
  private void rewind(TestHarness h, BufferFactory factory)
  {
    Buffer buf = null;

    buf = factory.newInstance();
    buf.position(5);
    buf.mark();
    buf.position(6);
    buf.limit(9);
    h.check(buf.rewind(), buf, "rewind: buf.rewind()");
    checkStatus(h, buf, "rewind", 20, 9, true, 9, 0);
    try
      {
        buf.reset();
        h.check(false, "rewind: mark not invalidated");
      }
      catch(InvalidMarkException ime)
      {
        h.check(true, "rewind: invalidated mark");
      }
  }
  private void clear(TestHarness h, BufferFactory factory)
  {
    Buffer buf = null;

    buf = factory.newInstance();
    buf.position(5);
    buf.mark();
    buf.position(6);
    buf.limit(7);
    h.check(buf.clear(), buf, "clear: buf.clear()");
    checkStatus(h, buf, "clear", 20, 20, true, 20, 0);
    try
      {
        buf.reset();
        h.check(false, "clear: mark not invalidated");
      }
      catch(InvalidMarkException ime)
      {
        h.check(true, "clear: invalidated mark");
      }
  }
  private void flip(TestHarness h, BufferFactory factory)
  {
    Buffer buf = null;

    buf = factory.newInstance();
    buf.position(5);
    buf.mark();
    buf.position(6);
    h.check(buf.flip(), buf, "flip: buf.flip()");
    checkStatus(h, buf, "flip", 20, 6, true, 6, 0);
    try
      {
        buf.reset();
        h.check(false, "flip: mark not invalidated");
      }
      catch(InvalidMarkException ime)
      {
        h.check(true, "flip: invalidated mark");
      }
  }


  private void checkStatus(TestHarness h, Buffer buf, String prefix,
    int cap, int lim, boolean hasRem, int rem, int pos)
  {
    h.check(buf.capacity(),     cap,    prefix +": buf.capacity()");
    h.check(buf.limit(),        lim,    prefix +": buf.limit()");
    h.check(buf.hasRemaining(), hasRem, prefix +": buf.hasRemaining()");
    h.check(buf.remaining(),    rem,    prefix +": buf.remaining()");
    h.check(buf.position(),     pos,    prefix +": buf.position()");
  }
}
