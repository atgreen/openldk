// Tags: not-a-test

package gnu.testlet.java.io.PipedStream;
import gnu.testlet.TestHarness;
import java.io.*;

class PipedStreamTestWriter implements Runnable
{

String str;
StringBufferInputStream sbis;
PipedOutputStream out;
TestHarness harness;

private boolean isReady = false;

public
PipedStreamTestWriter(TestHarness harness)
{
  this.harness = harness;

  str = "I went to work for Andersen Consulting after I graduated\n" +
     "from college.  They sent me to their training facility in St. Charles,\n" +
     "Illinois and tried to teach me COBOL.  I didn't want to learn it.\n" +
     "The instructors said I had a bad attitude and I got a green sheet\n" +
     "which is a nasty note in your file saying what a jerk you are.\n";

  sbis = new StringBufferInputStream(str);

  out = new PipedOutputStream();
}

public PipedOutputStream
getStream()
{
  return(out);
}

public String
getStr()
{
  return(str);
}

public synchronized void
waitTillReady()
{
  while (!isReady)
    {
      try
	{
	  this.wait();
	}
      catch (InterruptedException ie) { /* ignore */ }
    }
}

public void
run() 
{
  byte[] buf = new byte[32];

  int bytes_read;

  try
    {
      int b = sbis.read();
      out.write(b);

      synchronized(this)
	{
	  isReady = true;
	  this.notify();
	}

      while ((bytes_read = sbis.read(buf)) != -1)
        out.write(buf, 0, bytes_read);
	  out.flush();
      out.close();
    }
  catch(IOException e)
    {
      harness.debug("In writer: " + e);
      harness.check(false);
    }
}

}

