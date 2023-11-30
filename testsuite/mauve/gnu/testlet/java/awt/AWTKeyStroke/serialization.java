// Tags: JDK1.4

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.awt.AWTKeyStroke;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.AWTKeyStroke;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;

/**
 * Some checks for serialization of the {@link AWTKeyStroke} class.
 */
public class serialization implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    AWTKeyStroke ks1 = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK, true);
    AWTKeyStroke ks2 = null;

    try {
      ByteArrayOutputStream buffer = new ByteArrayOutputStream();
      ObjectOutput out = new ObjectOutputStream(buffer);
      out.writeObject(ks1);
      out.close();

      ObjectInput in = new ObjectInputStream(
        new ByteArrayInputStream(buffer.toByteArray())
      );
      ks2 = (AWTKeyStroke) in.readObject();
      in.close();
    }
    catch (Exception e) {
      harness.debug(e);
    }
    harness.check(ks1.equals(ks2));
  }
  
}
