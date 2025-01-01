// Tags: JDK1.4
// Uses: MultiplicationService MultiplierOne MultiplierTwo MultiplierThree

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

package gnu.testlet.javax.imageio.spi.ServiceRegistry;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URL;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.LogRecord;
import java.util.logging.SimpleFormatter;
import javax.imageio.spi.ServiceRegistry;


/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class lookupProviders
  implements Testlet
{
  public void test(TestHarness h)
  {
    Throwable caught;
    CustomClassLoader myClassLoader;
    Iterator it;
    URL[] providers;
    MultiplicationService s;

    // Setup a bridge between java.util.logging and the debug()
    // method of the passed Mauve TestHarness. Any log messages
    // will be visible when running Mauve with the -debug switch.
    setupLogging(h);

    String packageName
      = "gnu.testlet.javax.imageio.spi.ServiceRegistry";

    providers = new URL[2];
    try
      {
        providers[0] = createProviderList(new String[]
          {
            packageName + ".MultiplierOne",
            packageName + ".MultiplierTwo"
          });
        providers[1] = createProviderList(new String[]
          {
            packageName + ".MultiplierThree"
          });
      }
    catch (IOException ioex)
      {
        // There was some problem with the set-up.
        h.check(false);
        h.debug(ioex);
        return;
      }
    myClassLoader = new CustomClassLoader(providers);


    // Check #1: null spi --> IllegalArgumentException.
    try
      {
        ServiceRegistry.lookupProviders(null, myClassLoader);
        caught = null;
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    h.check(caught instanceof IllegalArgumentException);

    // Load some providers of MultiplicationService, while specifying
    // a custom class loader.
    it = ServiceRegistry.lookupProviders(MultiplicationService.class,
                                         myClassLoader);

    // Check #2.
    h.check(it.hasNext());
    
    // Check #3.
    s = (MultiplicationService) it.next();
    h.check(s.multiply(4, 5), 20);

    // Check #4.
    //
    // We intentionally do not perform an instanceof check, because
    // this would cause MultiplierOne to be loaded when this test case
    // gets resolved (at least on some JVMs). But then, check #5 would
    // fail.
    h.check(s.getName(), "MultiplierOne");

    // Check #5.
    h.check(it.hasNext());
    s = (MultiplicationService) it.next();

    // Check #6.
    h.check(s.getName(), "MultiplierTwo");

    // Check #7.
    h.check(it.hasNext());
    s = (MultiplicationService) it.next();

    // Check #8.
    h.check(s.getName(), "MultiplierThree");
    h.check(!it.hasNext());

    // Check #9.
    caught = null;
    try
      {
        it.next();
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    h.check(caught instanceof NoSuchElementException);
  }


  /**
   * A handler for <code>java.util.logging</code> that feeds any log
   * messages to the debug method of a Mauve TestHarness.
   *
   * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
   */
  private static class DebugHandler
    extends Handler
  {
    private final TestHarness harness;

    public DebugHandler(TestHarness harness)
    {
      this.harness = harness;
      setLevel(Level.ALL);
      setFormatter(new SimpleFormatter());
    }

    public void publish(LogRecord rec)
    {
      harness.debug(getFormatter().format(rec).trim());
      if (rec.getThrown() != null)
        harness.debug(rec.getThrown());
    }

    public void flush()
    {
    }

    public void close()
    {
    }
  }


  private void setupLogging(TestHarness harness)
  {
    Logger logger;

    logger = Logger.getLogger("gnu.classpath");
    logger.setLevel(Level.ALL);
    logger.addHandler(new DebugHandler(harness));
  }


  private URL createProviderList(String[] listedProviders)
    throws IOException
  {
    File f;
    PrintWriter p;
    
    f = File.createTempFile("MauveTest-", ".txt");
    f.deleteOnExit();
    p = new PrintWriter(new FileOutputStream(f));
    p.println("# This file has been generated by the Mauve testsuite");
    p.println("# while testing the mechanism for loading plug-in services.");
    p.println("# (Code in " + this.getClass().getName() + ").");
    p.println();
    p.println("# It should be automatically deleted from the temporary");
    p.println("# directory. If you happen to see this file, you probably");
    p.println("# have run the Mauve testsuite with a Java Virtual Machine");
    p.println("# that does not provide a correct implementaion of the method");
    p.println("# java.io.File.deleteOnExit().");
    p.println();
    p.println("# Implementations for the plug-in service");
    p.print("# ");
    p.println(MultiplicationService.class.getName());
    p.println();
    for (int i = 0; i < listedProviders.length; i++)
      p.println(listedProviders[i]);
    p.close();

    return f.toURL();
  }


  private class CustomClassLoader
    extends ClassLoader
  {
    private final URL[] providerLists;

    public CustomClassLoader(URL[] providerLists)
    {
      super(CustomClassLoader.class.getClassLoader());
      this.providerLists = providerLists;
    }


    private class ProviderEnumeration
      implements Enumeration
    {
      private int next = 0;

      public Object nextElement()
      {
        return providerLists[next++];
      }

      public boolean hasMoreElements()
      {
        return next < providerLists.length;
      }
    };


    protected Enumeration findResources(String name)
      throws IOException
    {
      if (name.equals("META-INF/services/gnu.testlet.javax.imageio.spi"
                      + ".ServiceRegistry.MultiplicationService"))
        return new ProviderEnumeration();
      else        
        return super.findResources(name);
    }
  }
}
