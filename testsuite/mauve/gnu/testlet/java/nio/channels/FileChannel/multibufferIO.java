/* mulitbufferIO.java -- Scatter/Gather IO on files
 Copyright (C) 2006 Michael Barker
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

// Tags: JDK14


package gnu.testlet.java.nio.channels.FileChannel;

import gnu.testlet.Testlet;

import java.nio.ByteBuffer;

/**
 * @author mike
 */
public class multibufferIO extends multidirectbufferIO
    implements Testlet
{

  private void initBuffer(ByteBuffer[] bs, byte[] data)
  {
    for (int i = 0; i < bs.length; i++)
      {
        bs[i] = ByteBuffer.wrap(data);
      }
  }

}
