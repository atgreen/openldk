// Tags: JDK1.4
// Uses: SocketCapturer

// Copyright (C) 2004 Sascha Brawer <brawer@dandelis.ch>

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

package gnu.testlet.java.util.logging.SocketHandler;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.SocketHandler;

/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class publish
  implements Testlet
{
  public void test(TestHarness th)
  {
    SocketCapturer capturer;
    SocketHandler handler;
    String captured = null;

    // Check #1.
    try
      {
        capturer = new SocketCapturer(0);
        handler = new SocketHandler("0.0.0.0", capturer.getLocalPort());
        handler.setLevel(Level.FINE);
        handler.publish(new LogRecord(Level.CONFIG, "hello, world"));
        handler.publish(new LogRecord(Level.FINER, "how are you?"));
        handler.close();
        captured = new String(capturer.getCaptured());
        th.check(true);
      }
    catch (Exception ex)
      {
        th.check(false);
        th.debug(ex);
        return;
      }

    // Check #2.
    th.check(captured.indexOf("<message>hello, world</message>") >= 0);

    // Check #3.
    th.check(captured.indexOf("<message>how are you?</message>") < 0);

    // Check #4.
    th.check(captured.indexOf("<?xml") == 0);

    // Check #5.
    th.check(captured.indexOf("</log>") >= 0);
  }
}
