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

package gnu.testlet.org.omg.CORBA.ORB;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.ORB.Asynchron.assServer;

import org.omg.CORBA.ORB;
import org.omg.CORBA.Request;
import org.omg.CORBA.TCKind;

import java.util.BitSet;
import java.util.Random;
import gnu.testlet.org.omg.CORBA.Asserter;

/**
 * This test checks if the server is able to handle parallel
 * submissions that are sent by
 * orb.send_multiple_requests_deferred. The server should handle
 * requests to the different objects in parallel threads.
 *
 * As the "task" is just to wait for the given duration,
 * it is possible to simulate a "distributed computing" on a single
 * processor machine and get the "acceleration factor". This
 * accelerator factor is checked for the minimal allowed value.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class parallelRunTest
  extends Asserter implements Testlet
{
  public void test(TestHarness harness)
  {
      h = harness;

      // 1. Testing the ORB methods.
      // Number of serving objects.
      int servers = 20;

      // Number of tasks to serve.
      int requests = 30;

      // Max tasks per object.
      int max_tasks = 2;

      String[] iors = assServer.start_server(servers);

      BitSet served = new BitSet();

      ORB orb = org.omg.CORBA.ORB.init(new String[ 0 ], null);

      // Make pause for the processes to start.
      try
        {
          Thread.sleep(500);
        }
      catch (InterruptedException ex)
        {
        }

      org.omg.CORBA.Object[] objects = new org.omg.CORBA.Object[ servers ];

      for (int i = 0; i < iors.length; i++)
        {
          objects [ i ] = orb.string_to_object(iors [ i ]);
        }

      int[] times = new int[ requests ];
      Random r = new Random();

      for (int i = 0; i < times.length; i++)
        {
          times [ i ] = r.nextInt(200) + 200;
          served.set(times [ i ]);
        }

      Request[] reqs = new Request[ requests ];

      // The server may limit number of the queued requests per
      // socket. We allow no more than 10 tasks per socket.
      int[] tasks = new int[ servers ];
      int rn;

      for (int i = 0; i < reqs.length; i++)
        {
          do
            {
              rn = r.nextInt(objects.length);
            }
          while (tasks [ rn ] > max_tasks);
          tasks [ rn ]++;

          // The object to handle the requrest is selected randomly.
          // The acceleration factor will be lower than the number
          // of the "parallelized objects". This is sufficient
          // as this is just a test, not a real distribute computing task.
          Request rq =
            objects [ rn ]._create_request(null, "sleep_and_return",
                                           orb.create_list(1), null
                                          );

          rq.set_return_type(orb.get_primitive_tc(TCKind.tk_long));
          rq.add_in_arg().insert_long(times [ i ]);
          reqs [ i ] = rq;
        }

      long started = System.currentTimeMillis();

      orb.send_multiple_requests_deferred(reqs);

      // Be sure the execution has returned correctly.
      assertTrue("Hangs on orb.send_multiple_requests_defferred",
                 System.currentTimeMillis() < started + 199
                );
      assertTrue("orb.send_multiple_requests_defferred:"+
                 "Cannot be ready immediately",
                 !orb.poll_next_response());

      for (int i = 0; i < reqs.length; i++)
        {
          try
            {
              Request a = orb.get_next_response();
              served.clear(a.result().value().extract_long());
            }
          catch (Exception ex)
            {
              ex.printStackTrace();
            }
        }

      // Check the acceleration factor.
      long done_in = System.currentTimeMillis() - started;

      long required = 0;
      for (int i = 0; i < times.length; i++)
        {
          required += times [ i ];
        }

      // Compute the acceleration factor * 100 %.
      int acceleration = (int) ((100 * required) / done_in);

      // With 20 virtual servers and the used algorithm, we should
      // expect at least five-fold acceleration.
      assertTrue("Parallel work is broken :" + acceleration, acceleration > 500);

      assertEquals("Not all tasks served", served.cardinality(), 0);

      // 2. Testing the Request methods.
      Request rq =
        objects [ 0 ]._create_request(null, "sleep_and_return",
                                      orb.create_list(1), null
                                     );

      rq.set_return_type(orb.get_primitive_tc(TCKind.tk_long));

      rq.add_in_arg().insert_long(200);

      long t = System.currentTimeMillis();
      rq.send_deferred();

      // Be sure the execution has returned correctly.
      assertTrue("Hangs on Request.send_defferred",
                 System.currentTimeMillis() < t + 199
                );
      assertTrue("Request.send_defferred:cannot be ready immediately",
                 !rq.poll_response());

      try
        {
          Thread.sleep(300);
        }
      catch (InterruptedException ex)
        {
        }

      assertTrue("Request.send_defferred:Must be ready now",
                 rq.poll_response());
      assertEquals("Request.send_defferred:Result must be ready",
                 rq.result().value().extract_long(), 200);

      orb.shutdown(true);
  }

}