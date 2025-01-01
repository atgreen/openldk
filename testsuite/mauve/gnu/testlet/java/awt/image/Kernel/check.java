// Test Kernel.check().

// Written by Jerry Quinn <jlquinn@optonline.net>

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

package gnu.testlet.java.awt.image.Kernel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.Kernel;


/**
 * @author <a href="mailto:jlquinn@optonline.net">Jerry Quinn</a>
 */
public class check implements Testlet
{
  public void test(TestHarness h)
  {
    float[] data = new float[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, };
    Kernel k = new Kernel(3, 4, data);
    h.check(k != null);
    h.check(k.getWidth() == 3);
    h.check(k.getHeight() == 4);
    h.check(k.getXOrigin() == 1);
    h.check(k.getYOrigin() == 1);

    float[] data1;
    try
      {
	data1 = k.getKernelData(null);
      }
    catch (IllegalArgumentException e)
      {
	data1 = new float[0];
	h.fail("Kernel.getKernelData");
      }

    h.checkPoint("Check kernel data");
    boolean ok = true;
    h.check(data1.length == data.length);
    for (int i=0; i < data1.length; i++)
      if (data[i] != data1[i])
	{
	  ok = false;
	  break;
	}
    h.check(ok == true);

    data1 = new float[12];
    try
      {
	data1 = k.getKernelData(data1);
      }
    catch (IllegalArgumentException e)
      {
	h.fail("Kernel.getKernelData");
      }

    ok = true;
    h.check(data1.length == data.length);
    for (int i=0; i < data1.length; i++)
      if (data[i] != data1[i])
	{
	  ok = false;
	  break;
	}
    h.check(ok == true);
    
    // Check failure modes
    h.checkPoint("Failure modes");
    ok = false;
    try
      {
	k.getKernelData(new float[1]);
      }
    catch (IllegalArgumentException e)
      {
	ok = true;
      }
    h.check(ok == true);

    ok = false;
    try
      {
	k = new Kernel(10, 10, data);
      }
    catch (IllegalArgumentException e)
      {
	ok = true;
      }
    h.check(ok == true);

    // Check that only the specified data gets copied
    data = new float[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14};
    k = new Kernel(3, 4, data);
    data1 = k.getKernelData(null);
    h.check(data1.length == 12);
    ok = true;
    for (int i=0; i < data1.length; i++)
      if (data[i] != data1[i])
	{
	  ok = false;
	  break;
	}
    h.check(ok == true);

  }
}
