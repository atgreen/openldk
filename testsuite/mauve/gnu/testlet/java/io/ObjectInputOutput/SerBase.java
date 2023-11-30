// Tags: not-a-test

/* SerBase.java -- Base class that defines a field 'a'.

   Copyright (c) 2002 by Free Software Foundation, Inc.
   Written by Mark Wielaard (mark@klomp.org).
   Based on a test by Jeroen Frijters (jeroen@sumatra.nl).

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation, version 2. (see COPYING)

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation
   Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA */

package gnu.testlet.java.io.ObjectInputOutput;

import java.io.*;

class SerBase implements Serializable
{
  private int a;

  SerBase(int a)
  {
    this.a = a;
  }

  int getA()
  {
    return a;
  }
}
