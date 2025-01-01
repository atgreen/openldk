// Tags: JDK1.4
// Copyright (C) 2005 Audrius Meskauskas (AudriusA@Bioinformatics.org)
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


package gnu.testlet.org.omg.IOP.IOR;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.ORB.communication.comServant;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.portable.OutputStream;
import org.omg.IOP.*;
import org.omg.IOP.IOR;
import org.omg.IOP.IORHelper;

/**
 * The IOR profiles are used to transfer the object reference between
 * two ORBs (over network and so on). That is written by IORHelper,
 * must be understood by portable.InputStream, that is written by
 * portable.OutputStream must be understood by IORHelper. The test
 * verifies this. As a sample of the object with IOR reference it uses
 * comServant from another test.
 *
 * Error message notation:
 *  HW - IORHelper writes, InputStream reads.
 *  HR - IORHelper reads, InputStream writes.
 *
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public class Streams
  implements Testlet
{
  public void test(TestHarness h)
  {
    ORB orb = ORB.init(new String[ 0 ], null);
    Any an = orb.create_any();

    OutputStream out = an.create_output_stream();

    comServant object = new comServant();
    orb.connect(object);

    out.write_Object(object);

    IOR ior = IORHelper.read(out.create_input_stream());

    boolean ip = false;

    for (int i = 0; i < ior.profiles.length; i++)
      {
        if (ior.profiles [ i ].tag == TAG_INTERNET_IOP.value)
          {
            if (ip)
              h.fail("HR:One internet profile expected");
            ip = true;
            h.check(ior.profiles [ i ].profile_data.length > 0,
                    "HR:Internet profile data"
                   );
          }
      }
    h.check(ip, "HR:Internet profile present");

    h.check(object._is_a(ior.type_id), "HR id");
    h.check(ior.profiles.length > 0, "HR profiles");

    out = orb.create_any().create_output_stream();

    IORHelper.write(out, ior);

    org.omg.CORBA.Object obj = out.create_input_stream().read_Object();

    h.check(obj._is_a(ior.type_id), " HW id");
    h.check(ior.profiles.length > 0, "HW profiles");

    ip = false;

    for (int i = 0; i < ior.profiles.length; i++)
      {
        if (ior.profiles [ i ].tag == TAG_INTERNET_IOP.value)
          {
            if (ip)
              h.fail("HW:One internet profile expected");
            ip = true;
            h.check(ior.profiles [ i ].profile_data.length > 0,
                    "HW:Internet profile data"
                   );
          }
      }

  }
}