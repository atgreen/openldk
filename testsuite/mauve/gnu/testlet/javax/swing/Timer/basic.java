// Tags: JDK1.2

// Copyright (C) 2004 Audrius Meskauskas <audriusa@bluewin.ch>

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

package gnu.testlet.javax.swing.Timer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.*;

import javax.swing.Timer;

/**
 * THIS TEST NEEDS FIVE SECONDS TO COMPLETE!
 * The basic test of the swing timer.
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class basic
  implements Testlet, ActionListener
{
  /**
   * The acceptable timer work accuracy in %.
   */
  public static int ACCEPTABLE_ACCURACY = 20;

  static int EXPECTED_DELAY = 800;
  static int EXPECTED_INITIAL_DELAY = 2000;

  long history[] = new long[ 200 ];
  int p = 1;
  long started;
  Thread main;

  public void test(TestHarness harness)
  {
    main = Thread.currentThread();

    Timer t = new Timer(EXPECTED_DELAY, this);

    t.setCoalesce(false);

    t.setInitialDelay(EXPECTED_INITIAL_DELAY);
    t.setDelay(EXPECTED_DELAY);

    System.gc();

    history [ 0 ] = System.currentTimeMillis();

    t.start();

    try
      {
        Thread.sleep(5000);
      }
    catch (InterruptedException iex)
      {
      }

    t.stop();

    double S = 0;
    long d;
    StringBuffer series = new StringBuffer();


    for (int i = 1; i < p; i++)
      {
        d = Math.abs((history [ i ] - history [ i - 1 ]) -
                     (i == 1 ? EXPECTED_INITIAL_DELAY : EXPECTED_DELAY)
                    );
        series.append((history [ i ] - history [ i - 1 ])+" ");
        S += d;
      }

    S = S / (p - 1);

    int percentError = (int) (100 * S / EXPECTED_DELAY);

    if (percentError > ACCEPTABLE_ACCURACY)
      harness.fail("Inaccurate work, series "+series+", "
      +percentError+
      " % deviation from  2000 800 ..");
  }

  public void actionPerformed(ActionEvent parm1)
  {
    try
      {
        history [ p ] = System.currentTimeMillis();
        p++;
        Thread.sleep( (long) Math.random()*EXPECTED_DELAY );
      }
    catch (ArrayIndexOutOfBoundsException ex)
      {
        // Should never happen during the normal work, but
        // is catched here to prevent test from the possible hanging.
        main.interrupt();
      }
    catch (InterruptedException iex)
      {
        main.interrupt();
      }
  }
}
