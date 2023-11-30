// Tags: JDK1.2

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

package gnu.testlet.org.omg.CORBA.portable.OutputStream;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.Asserter;

import org.omg.CORBA.ORB;
import org.omg.CORBA.OctetSeqHelper;
import org.omg.CORBA.Request;
import org.omg.CORBA.TCKind;

/**
 * This tests uses a non standard CORBA object that accepts the
 * passed parameters and then returns all passed data as a
 * plain byte array, exactly how they were sent. The returned
 * sequence can be byte to byte compared with the sequence,
 * expected from the OMG CORBA specification. The test is against
 * loss of interoperability by modifications that may affect
 * both input and output CDR streams, leaving the implementation
 * working with self, but interoperable.
 *
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public class BinaryAlignment
  extends Asserter
  implements Testlet
{
  // The correct data sequence, excluding the message header and request
  // header, as it should be deciding from CORBA spefication for GIOP 1.2
  static final int[] expected =
    new int[]
    {
      0x77, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
      0x2, 0x0, 0x3, 0x0, 0x0, 0x0, 0x0, 0x0, 0xb, 0x5b, 0x73, 0x74, 0x72, 0x69,
      0x6e, 0x67, 0x20, 0x34, 0x5d, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x40, 0x46,
      0xd8, 0x31, 0x26, 0xe9, 0x78, 0xd5, 0xfe, 0xff
    };

  public void test(TestHarness harness)
  {
    h = harness;

    String[] args = new String[ 0 ];

    final ORB server_orb = ORB.init(args, null);

    mirror reflector = new mirror();
    server_orb.connect(reflector);

    new Thread()
      {
        public void run()
        {
          server_orb.run();
        }
      }.start();

    // Wait for 500 ms for the orb to start.
    try
      {
        Thread.sleep(500);
      }
    catch (InterruptedException ex)
      {
      }

    String ior = server_orb.object_to_string(reflector);

    // Instantiate another orb where this reflector will be a stub:
    ORB client_orb = ORB.init(args, null);

    org.omg.CORBA.Object object =
      (org.omg.CORBA.Object) client_orb.string_to_object(ior);

    Request r =
      object._create_request(null, "pass", server_orb.create_list(0), null);

    // Write the test values.
    r.add_in_arg().insert_octet((byte) 0x77);
    r.add_in_arg().insert_long(1);
    r.add_in_arg().insert_longlong(2);
    r.add_in_arg().insert_short((short) 3);
    r.add_in_arg().insert_string("[string 4]");
    r.add_in_arg().insert_double(45.689);
    r.add_in_arg().insert_octet((byte) 0xFE);

    // This will serve as EOF marker.
    r.add_in_arg().insert_octet((byte) 0xFF);

    // For the last parameter, the value is not set.
    r.set_return_type(server_orb.create_sequence_tc(0,
                                                    server_orb.get_primitive_tc(TCKind.tk_octet)
                                                   )
                     );

    r.invoke();

    byte[] reflection = OctetSeqHelper.extract(r.result().value());

    assertEquals("length", expected.length, reflection.length);

    for (int i = 0; i < reflection.length; i++)
      {
        if ((reflection [ i ] & 0xFF) != expected [ i ])
          fail("Mismatch [" + i + "] expected " +
               Integer.toHexString(expected [ i ]) + " actual " +
               Integer.toHexString(0xFF & reflection [ i ])
              );
      }

    client_orb.shutdown(false);
    server_orb.shutdown(false);

    client_orb.destroy();
    server_orb.destroy();
  }
}