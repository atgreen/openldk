// Tags: not-a-test

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

package gnu.testlet.java.util.logging.Handler;

import java.util.logging.Handler;
import java.util.logging.LogRecord;


/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class TestHandler
  extends Handler
{
  public TestHandler()
  {
  }

  public void flush()
  {
  }

  public void close()
  {
  }

  public void publish(LogRecord record)
  {
  }


  /**
   * Invokes the reportError method, which has protected access
   * and cannot be called from the outside.
   */
  public void invokeReportError(String msg, Exception ex, int code)
  {
    reportError(msg, ex, code);
  }
}
