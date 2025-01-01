// Files: logging.properties

package gnu.testlet.java.util.logging.LogManager;

import gnu.testlet.*;
import java.io.*;
import java.util.logging.*;

// Test case for setting Logger handlers using resource property
public class readConfiguration implements Testlet {
    
  public void test(TestHarness harness) {
    LogManager manager = LogManager.getLogManager();
    
    // This resource contains a "handlers = TestHandler" property
    harness.checkPoint("read user logging properties");
    try
      {
	InputStream is = 
	  ClassLoader.getSystemResourceAsStream("gnu/testlet/java/util/logging/LogManager/logging.properties");
	manager.readConfiguration(is);
	harness.check(true);
	is.close();
      }
    catch (IOException e)
      {
	harness.check(false);
	harness.debug(e);
      }
    
    Logger logger = Logger.getLogger("TestLogger");
    Logger logger2 = logger;
    logger2.setLevel(Level.FINE);
    
    // This just copied from
    // org.apache.commons.logging.jdk14.CustomConfigTestCase, where I noticed
    // this bug
    while (logger.getParent() != null) {
      logger = logger.getParent();
    }
    
    // I gather that handles should now = {TestHandler handler}
    Handler[] handlers = logger.getHandlers();
    
    harness.checkPoint("handlers is not null");
    harness.check(handlers != null);
    
    // But does it?
    if (handlers != null)
      {
	harness.checkPoint("handlers length");
	harness.check(handlers.length, 1);
	harness.check(handlers[0].getClass() == TestHandler.class);
	
	TestHandler handler = (TestHandler) handlers[0];

	harness.checkPoint("state reset");

	harness.check(logger2.getLevel() == Level.FINE);

	try
	  {
	    manager.readConfiguration();
	    harness.check(true);
	  }
	catch (Exception e)
	  {
	    harness.check(false);
	    harness.debug(e);
	  }
	harness.check(logger2.getLevel() == null);
	harness.check(handler.isClosed);
      }
  }
}
