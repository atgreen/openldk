// Tags: not-a-test

package gnu.testlet.java.io.PipedReaderWriter;
import gnu.testlet.TestHarness;
import java.io.*;

class PipedTestWriter implements Runnable
{

String str;
StringReader sbr;
PipedWriter out;
TestHarness harness;

public
PipedTestWriter(TestHarness harness)
{
  this.harness = harness;

  str = "In college, there was a tradition going for a while that people\n" +
    "would get together and hang out at Showalter Fountain - in the center\n" +
    "of Indiana University's campus - around midnight.  It was mostly folks\n" +
    "from the computer lab and just people who liked to use the Forum\n" +
    "bbs system on the VAX.  IU pulled the plug on the Forum after I left\n" +
    "despite its huge popularity.  Now they claim they are just giving\n" +
    "students what they want by cutting deals to make the campus all\n" +
    "Microsoft.\n";

  sbr = new StringReader(str);

  out = new PipedWriter();
}

public PipedWriter
getWriter()
{
  return(out);
}

public String
getStr()
{
  return(str);
}

public void
run() 
{
  char[] buf = new char[32];

  int chars_read;

  try
    {
      int b = sbr.read();
      out.write(b);

      while ((chars_read = sbr.read(buf)) != -1)
        out.write(buf, 0, chars_read);
	  out.flush();
      out.close();
    }
  catch(IOException e)
    {
      harness.debug("In Writer: " + e);
      harness.check(false);
    }
}

} // PipedTestWriter

