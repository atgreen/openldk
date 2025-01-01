// Tags: not-a-test

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.io.Serializable;

import java.io.ObjectStreamException;
import java.io.Serializable;

/**
 * A test class.
 */
public class MySerializable implements Serializable {

  /** A flag the tracks whether the readResolved() method is called. */
  private boolean resolved;
  
  /**
   * Default constructor.
   */
  public MySerializable() 
  {
    this.resolved = false;  
  }

  /**
   * Returns the flag that indicates whether or not the readResolved() method
   * has been called.
   * 
   * @return A boolean.
   */
  public boolean isResolved() 
  {
    return this.resolved; 
  }
  
  /**
   * This method should be called by the serialization mechanism.
   * 
   * @return A resolved object.
   * 
   * @throws ObjectStreamException
   */
  private Object readResolve() throws ObjectStreamException 
  {
    this.resolved = true;
    return this;
  }
  
}