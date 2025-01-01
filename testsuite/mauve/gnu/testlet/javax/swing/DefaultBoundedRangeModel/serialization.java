/* serialization.java -- some checks for object serialization.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

package gnu.testlet.javax.swing.DefaultBoundedRangeModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;

import javax.swing.DefaultBoundedRangeModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class serialization implements Testlet, ChangeListener
{
  public void stateChanged(ChangeEvent e) 
  {
    // TODO Auto-generated method stub
  }

  public void test(TestHarness harness)
  {
      DefaultBoundedRangeModel m1 = new DefaultBoundedRangeModel(1, 2, 0, 99);
      m1.addChangeListener(this);
      DefaultBoundedRangeModel m2 = null;

      try {
          ByteArrayOutputStream buffer = new ByteArrayOutputStream();
          ObjectOutput out = new ObjectOutputStream(buffer);
          out.writeObject(m1);
          out.close();

          ObjectInput in = new ObjectInputStream(new ByteArrayInputStream(
                  buffer.toByteArray()));
          m2 = (DefaultBoundedRangeModel) in.readObject();
          in.close();
      }
      catch (Exception e) {
          e.printStackTrace();
      }
           
      harness.check(m1.getValue(), m2.getValue());
      harness.check(m1.getMinimum(), m2.getMinimum());
      harness.check(m1.getMaximum(), m2.getMaximum());
      harness.check(m1.getExtent(), m2.getExtent());
      
      // the listeners are not restored
      harness.check(m1.getChangeListeners().length, 1);
      harness.check(m2.getChangeListeners().length, 0);
  }
}
