/* ConcurrentLogging.java -- Test Concurrent access to Logger
   Copyright (C) 2008 Mario Torre <neugens@limasoftware.net>
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

package gnu.testlet.java.util.logging.Logger;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Fix PR35974, when multiple threads try to access methods or Logger.
 * 
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class PR35974
  implements Testlet
{
  private static final boolean DEBUG = false;
  
  private static Logger parentLogger =
    Logger.getLogger("new caprica event logger");
  private TestHarness harness = null;
  static volatile boolean fighting = true;
  
  public void test(TestHarness harness)
  {
    this.harness = harness;
  
    HarnessHandler handler = new HarnessHandler();
    
    LoggerThread galactica = new LoggerThread("Galactica", handler);
    
    LoggerThread cylonBasestar1 =
      new LoggerThread("Cylon Basestar #1", handler);
    LoggerThread cylonBasestar2 =
      new LoggerThread("Cylon Basestar #2", handler);
    LoggerThread cylonBasestar3 =
      new LoggerThread("Cylon Basestar #3", handler);
    
    galactica.start();
    
    cylonBasestar1.start();
    cylonBasestar2.start();
    cylonBasestar3.start();
    
    BattlestarPegasusThread pegasus = new BattlestarPegasusThread();
    pegasus.start();
  }
  
  private class HarnessHandler extends Handler
  {
    public void close() throws SecurityException
    {
      /* does nothing */
    }

    @Override
    public void flush()
    {
      /* does nothing */
    }

    public void success()
    {
      harness.check(true, Thread.currentThread().getName());
    }
    
    @Override
    public void publish(LogRecord record)
    {
      if (DEBUG == false)
        return;
      
      // this is not correct in real world code, but it's ok for our
      // simple test
      if (Thread.currentThread().getName().equalsIgnoreCase("Galactica"))
        harness.debug(record.getMessage() + "--------------------------->");
      else
        harness.debug("\t\t\t\t\t\t\t\t\t\t\t\t\t<---------------------------"
                      + record.getMessage());
    }
    
  }
  
  private static class BattlestarPegasusThread extends Thread
  {
    private long startTime = System.currentTimeMillis();
    
    public BattlestarPegasusThread()
    {
      super("Pegasus to the rescue...");
    }
    
    @Override
    public void run()
    {
      parentLogger.log(Level.INFO, this.getName());
      
      long stopTime = System.currentTimeMillis();
      while ((stopTime - startTime) < 30000)
        {
          stopTime = System.currentTimeMillis();
        }
      fighting = false;
      
      parentLogger.log(Level.INFO, "Pegasus destroyed...");
    }
  }
 
  private static class LoggerThread extends Thread
  {
     private static Logger brokenLogger = null;
    
    private HarnessHandler handler = null;
        
    public LoggerThread(String name, HarnessHandler handler)
    {
      super(name);
      super.setDaemon(true);
      this.handler = handler;
    }
    
    @Override
    public void run()
    {
      parentLogger.log(Level.INFO, this.getName() +
                       " did the jump into new caprica orbit ");
      
      while (fighting)
        {
          // These methods are all synchronized in the implementation.
          // Of course, you see the problem, there is no synchronization
          // between the first instruction and the others, and you can see
          // this because some output from the logger is still left on even when
          // we setLevel(Level.OFF), this is because of some concurrent access,
          // but it's ok, because what we do here is to force a concurrent
          // access somehow hoping for a deadlock at some point, which will
          // not happen because the locking system on the Logger class has been
          // improved, but still will left our brokenLogger in a somewhat
          // confused state (that is, it works only because handler and
          // parentLogger are always the same, otherwise it would not work)
          // This code is broken by design, so don't try to imitate this style
          // in real word code, please.
          brokenLogger = Logger.getLogger(this.getName());
          
          if (DEBUG == false)
            brokenLogger.setLevel(Level.OFF);
          else
            brokenLogger.setParent(parentLogger);
          
          brokenLogger.addHandler(handler);
          brokenLogger.log(Level.INFO,  this.getName() + " fires");
        }
      
      parentLogger.log(Level.INFO, this.getName() + " jumps");
      handler.success();
    }
  }
}
