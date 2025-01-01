// Tags: JDK1.4
// Uses: ByteBufferFactory

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

package gnu.testlet.java.nio.ByteBuffer;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.nio.BufferOverflowException;
import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.InvalidMarkException;

public class Allocating implements Testlet
{
  public void test(TestHarness h)
  {
  	//
  	// allocate(int)
  	//
    h.checkPoint("allocate(int)");
  	h.check(true);
    ByteBufferFactory allocateFactory = new ByteBufferFactory()
    {
      public ByteBuffer newInstance()
      {
        return ByteBuffer.allocate(10);
      }
    };
    ByteBuffer bufAll = ByteBuffer.allocate(10);
    h.check(bufAll.isDirect(), false, "isDirect()");
    h.check(bufAll.hasArray(), "hasArray()");
    h.check(bufAll.arrayOffset(), 0, "arrayOffset()");
    h.check(bufAll.array() != null, "array()");

    overflow(h, allocateFactory, 10);
    underflow(h, allocateFactory, 10);
    compact(h, allocateFactory, 10);

  	//
  	// allocateDirect(int)
  	//
  	h.checkPoint("allocateDirect(int)");
  	h.check(true);
  	ByteBufferFactory allocateDirectFactory = new ByteBufferFactory()
	{
  	  public ByteBuffer newInstance()
  	  {
  	  	return ByteBuffer.allocateDirect(10);
  	  }
	};
    ByteBuffer bufAllDir = ByteBuffer.allocateDirect(10);
    h.check(bufAllDir.isDirect(), true, "isDirect()");
    // it's unspecified if this buffer will have backing array so we test it if there's one
    if (bufAllDir.hasArray())
    {
      h.check(bufAllDir.arrayOffset(), 0, "arrayOffset()");
      h.check(bufAllDir.array() != null, "array()");
    }

    overflow(h, allocateDirectFactory, 10);
    underflow(h, allocateDirectFactory, 10);
    compact(h, allocateDirectFactory, 10);

  	//
  	// wrap(byte[])
  	//
  	h.checkPoint("wrap(byte[])");
  	h.check(true);
  	ByteBufferFactory wrapFactory = new ByteBufferFactory()
	{
  	  public ByteBuffer newInstance()
  	  {
  	  	return ByteBuffer.wrap(new byte[10]);
  	  }
	};
	byte[] arrWrap = new byte[10];
    ByteBuffer bufWrap = ByteBuffer.wrap(arrWrap);
    h.check(bufWrap.isDirect(), false, "isDirect()");
    h.check(bufWrap.hasArray(), true, "hasArray()");
    h.check(bufWrap.arrayOffset(), 0, "arrayOffset()");
    h.check(bufWrap.array(), arrWrap, "array()");

    overflow(h, wrapFactory, 10);
    underflow(h, wrapFactory, 10);
    compact(h, wrapFactory, 10);
 
  	//
  	// wrap(byte[], int, int)
  	//
  	h.checkPoint("wrap(byte[], int, int)");
  	h.check(true);
  	ByteBufferFactory wrapWithOffsetFactory = new ByteBufferFactory()
	{
  	  public ByteBuffer newInstance()
  	  {
  	  	return ByteBuffer.wrap(new byte[20], 5, 10);
  	  }
	};
	byte[] arrWrapOff = new byte[10];
    ByteBuffer bufWrapOff = ByteBuffer.wrap(arrWrapOff, 1, 1);
    h.check(bufWrapOff.isDirect(), false, "isDirect()");
    h.check(bufWrapOff.hasArray(), true, "hasArray()");
    h.check(bufWrapOff.arrayOffset(), 0, "arrayOffset()");
    h.check(bufWrapOff.array(), arrWrapOff, "array()");

    overflow(h, wrapWithOffsetFactory, 15);
    underflow(h, wrapWithOffsetFactory, 15);
    compact(h, wrapWithOffsetFactory, 20);

    array(h);
    synchWrappedBufferWithArray(h);
  }

  private void overflow(TestHarness h, ByteBufferFactory factory, int limit)
  {
    ByteBuffer buf = null;

    buf = factory.newInstance();
    buf.position(limit - 1);
    buf.put((byte)0x01);
    try
		{
    	buf.put((byte)0x01);
    	h.check(false, "byte overflow");
		}
    	catch(BufferOverflowException boe)
			{
    		h.check(true, "byte overflow");
			}

    buf = factory.newInstance();
    buf.position(limit - 3);
    buf.putShort((short)0x0101);
    try
		{
    	buf.putShort((short)0x0101);
    	h.check(false, "short overflow");
		}
    	catch(BufferOverflowException boe)
			{
    		h.check(true, "short overflow");
			}

    buf = factory.newInstance();
    buf.position(limit - 6);
    buf.putInt(0x01010101);
    try
		{
    	buf.putInt(0x01010101);
    	h.check(false, "int overflow");
		}
    	catch(BufferOverflowException boe)
			{
    		h.check(true, "int overflow");
			}

    buf = factory.newInstance();
    buf.position(limit - 9);
    buf.putLong(0x0101010101010101L);
    try
		{
    	buf.putLong(0x0101010101010101L);
    	h.check(false, "long overflow");
		}
    	catch(BufferOverflowException boe)
			{
    		h.check(true, "long overflow");
			}

    buf = factory.newInstance();
    buf.position(limit - 6);
    buf.putFloat(1.0f);
    try
		{
    	buf.putFloat(1.0f);
    	h.check(false, "float overflow");
		}
    	catch(BufferOverflowException boe)
			{
    		h.check(true, "float overflow");
			}

    buf = factory.newInstance();
    buf.position(limit - 9);
   	buf.putDouble(1.0);
    try
		{
    	buf.putDouble(1.0);
    	h.check(false, "double overflow");
		}
    	catch(BufferOverflowException boe)
			{
    		h.check(true, "double overflow");
			}

    buf = factory.newInstance();
    buf.position(limit - 3);
    buf.putChar('\u0101');
    try
		{
    	buf.putChar('\u0101');
    	h.check(false, "char overflow");
		}
    	catch(BufferOverflowException boe)
			{
    		h.check(true, "char overflow");
			}
  }
  private void underflow(TestHarness h, ByteBufferFactory factory, int limit)
  {
    ByteBuffer buf = null;

    buf = factory.newInstance();
    buf.position(limit - 1);
    buf.get();
    try
		{
    	buf.get();
    	h.check(false, "byte underflow");
		}
    	catch(BufferUnderflowException boe)
			{
    		h.check(true, "byte underflow");
			}

    buf = factory.newInstance();
    buf.position(limit - 3);
    buf.getShort();
    try
		{
    	buf.getShort();
    	h.check(false, "short underflow");
		}
    	catch(BufferUnderflowException boe)
			{
    		h.check(true, "short underflow");
			}

    buf = factory.newInstance();
    buf.position(limit - 6);
    buf.getInt();
    try
		{
    	buf.getInt();
    	h.check(false, "int underflow");
		}
    	catch(BufferUnderflowException boe)
			{
    		h.check(true, "int underflow");
			}

    buf = factory.newInstance();
    buf.position(limit - 9);
    buf.getLong();
    try
		{
    	buf.getLong();
    	h.check(false, "long underflow");
		}
    	catch(BufferUnderflowException boe)
			{
    		h.check(true, "long underflow");
			}

    buf = factory.newInstance();
    buf.position(limit - 6);
    buf.getFloat();
    try
		{
    	buf.getFloat();
    	h.check(false, "float underflow");
		}
    	catch(BufferUnderflowException boe)
			{
    		h.check(true, "float underflow");
			}

    buf = factory.newInstance();
    buf.position(limit - 9);
   	buf.getDouble();
    try
		{
    	buf.getDouble();
    	h.check(false, "double underflow");
		}
    	catch(BufferUnderflowException boe)
			{
    		h.check(true, "double underflow");
			}

    buf = factory.newInstance();
    buf.position(limit - 3);
    buf.getChar();
    try
		{
    	buf.getChar();
    	h.check(false, "char underflow");
		}
    	catch(BufferUnderflowException boe)
			{
    		h.check(true, "char underflow");
			}
  }

  private void compact(TestHarness h, ByteBufferFactory factory, int size)
  {
    h.checkPoint("compact()");

    ByteBuffer buf = null;

    buf = factory.newInstance();
    buf.rewind();
    for (int i = 0; i < 10; i++)
    {
      buf.put((byte)(i + 1));
    }
    buf.limit(6);
    buf.position(1);
    buf.mark();
    buf.get();
    
    h.check(buf.compact(), buf, "compact() return value");
    h.check(buf.position(), 4, "compact()/position");
    h.check(buf.limit(), size, "compact()/limit");
    try
      {
        buf.reset();
        h.check(false, "mark: mark not invalidated");
      }
      catch(InvalidMarkException ime)
      {
        h.check(true, "mark: invalidated mark");
      }
    h.checkPoint("compact()/contents");
    buf.rewind();
    h.check(buf.get(), 3);
    h.check(buf.get(), 4);
    h.check(buf.get(), 5);
    h.check(buf.get(), 6);
  }


  private void array(TestHarness h)
  {
  	byte[] arr = null;
    ByteBuffer buf = null;

    h.checkPoint("array");
    arr = new byte[] { 1, 2, 3 };
    buf = ByteBuffer.wrap(arr);
    h.check(buf.array(), arr, "array");

    
  }

  private void synchWrappedBufferWithArray(TestHarness h)
  {
  	byte[] arr = null;
    ByteBuffer buf = null;

    h.checkPoint("synchWrappedBufferWithArray/wrap(byte[])");
    arr = new byte[10];
    buf = ByteBuffer.wrap(arr);
    for (int i = 0; i < arr.length; i++)
    {
      arr[i] = (byte)(i + 1);
    }
    buf.order(ByteOrder.BIG_ENDIAN);
    h.check(buf.getShort(), (short)0x0102);
    buf.putShort((short)0x0b0c);
    buf.order(ByteOrder.LITTLE_ENDIAN);
    h.check(buf.getShort(), (short)0x0605);
    buf.putShort((short)0x0d0e);
    h.check(arr[2], 0x0b);
    h.check(arr[3], 0x0c);
    h.check(arr[6], 0x0e);
    h.check(arr[7], 0x0d);

    h.checkPoint("synchWrappedBufferWithArray/wrap(byte[], int, int)");
    arr = new byte[10];
    buf = ByteBuffer.wrap(arr, 2, 8);
    for (int i = 0; i < arr.length; i++)
    {
      arr[i] = (byte)(i + 1);
    }
    buf.order(ByteOrder.BIG_ENDIAN);
    h.check(buf.getShort(), (short)0x0304);
    buf.putShort((short)0x0b0c);
    buf.order(ByteOrder.LITTLE_ENDIAN);
    h.check(buf.getShort(), (short)0x0807);
    buf.putShort((short)0x0d0e);
    h.check(arr[4], 0x0b);
    h.check(arr[5], 0x0c);
    h.check(arr[8], 0x0e);
    h.check(arr[9], 0x0d);
  }
}
