// Tags: JDK1.4

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

package gnu.testlet.java.util.logging.XMLFormatter;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.XMLFormatter;

import java.util.TimeZone;

/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class formatMessage
  implements Testlet
{
  public void test(TestHarness h)
  {
    XMLFormatter formatter = new XMLFormatter();
    LogRecord rec;

    // Check #1.
    try
      {
        formatter.formatMessage(null);
        h.check(false);
      }
    catch (NullPointerException _)
      {
        h.check(true);
      }
    catch (Exception _)
      {
        h.check(false);
      }


    // Check #2.
    rec = new LogRecord(Level.INFO, "foobar");
    
    //Need to force the default time zone to UTC or else
    //the comparison uses system time zone and makes the tests
    //break.
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    rec.setMillis(1234567);
    rec.setSequenceNumber(42);
    rec.setThreadID(21);
    h.check(formatter.format(rec),
            EXPECTED_PREFIX
            + "  <thread>21</thread>\n"
            + "  <message>foobar</message>\n"
            + "</record>\n");


    // Check #3.
    rec.setSourceClassName(
      "FakeClass");
    rec.setSourceMethodName("test(fake)");
    h.check(formatter.format(rec),
            EXPECTED_PREFIX
            + "  <class>FakeClass</class>\n"
            + "  <method>test(fake)</method>\n"
            + "  <thread>21</thread>\n"
            + "  <message>foobar</message>\n"
            + "</record>\n");


    // Check #4.
    rec.setMessage("foobar {1}-{0}");
    rec.setParameters(new String[] { "peace", "love" });
    h.check(formatter.format(rec),
            EXPECTED_PREFIX
            + "  <class>FakeClass</class>\n"
            + "  <method>test(fake)</method>\n"
            + "  <thread>21</thread>\n"
            + "  <message>foobar love-peace</message>\n"
            + "</record>\n");


    // Check #5.
    rec.setThrown(new TestException("non-localized message"));
    rec.setMessage("mauve is a beautiful color");
    h.check(deleteFrames(formatter.format(rec)),
            EXPECTED_PREFIX
            + "  <class>FakeClass</class>\n"
            + "  <method>test(fake)</method>\n"
            + "  <thread>21</thread>\n"
            + "  <message>mauve is a beautiful color</message>\n"
            + "  <exception>\n"
            + "    <message>gnu.testlet.java.util.logging"
            + ".XMLFormatter.formatMessage$TestException: localized "
            + "message</message>\n"
            + "  </exception>\n"
            + "</record>\n");

    // Check #6.
    rec.setMessage("ENTRY {0}");
    rec.setParameters(new String[] { "foo.bar" });
    rec.setResourceBundleName(TestResourceBundle.class.getName());
    rec.setThrown(null);
    h.check(formatter.format(rec),
            EXPECTED_PREFIX
            + "  <class>FakeClass</class>\n"
            + "  <method>test(fake)</method>\n"
            + "  <thread>21</thread>\n"
            + "  <message>ENTRY foo.bar</message>\n"
            + "</record>\n");
  }


  //1234567 milliseconds is only 20 minutes and 
  //34 seconds (past the Epoch, UTC time).
  private static final String EXPECTED_PREFIX =
     "<record>\n"
    + "  <date>1970-01-01T00:20:34</date>\n"
    + "  <millis>1234567</millis>\n"
    + "  <sequence>42</sequence>\n"
    + "  <level>INFO</level>\n";


  private static String deleteFrames(String s)
  {
    int start, end;
    StringBuffer buf;
    
    start = s.indexOf("  <frame");
    end = s.lastIndexOf("</frame>\n  ");
    if (start < 0 || end < 0)
      return s;

    buf = new StringBuffer(s);
    buf.delete(start, end + "</frame>\n  ".length());
    return buf.toString();
  }


  private static class TestException extends Exception
  {
    public TestException()
    {
    }

    public TestException(String s)
    {
      super(s);
    }

    public String getLocalizedMessage()
    {
      return "localized message";
    }
  };


  public static class TestResourceBundle
    extends java.util.ListResourceBundle
  {
    public TestResourceBundle()
    {
    }

    private final Object[][] contents = new Object[][]
    {
      { "ENTRY {0}", "BETRETEN {0}" }
    };

    protected Object[][] getContents()
    {
      return contents;
    }
  };

}
