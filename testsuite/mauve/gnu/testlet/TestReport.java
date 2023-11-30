// Copyright (c) 2004 Noa Resare.
// Written by Noa Resre <noa@resare.com>
									       
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

package gnu.testlet;

import java.util.*;
import java.io.*;

/**
 * A TestReport represents all the results of a test run. The TestReport
 * can be serialized to xml with the writeXml method.
 */
public class TestReport
{
  private Properties systemProperties;
  private List<TestResult> testResults;

  private static final String ENCODING = "UTF-8";


  /**
   * Creates a new TestReport object with jvmName and jvmVersion set.
   *
   * @param systemProperties the Properties object returned from
   * System.getProperties() of the jvm that is tested.
   */
  public TestReport(Properties systemProperties)
  {
    this.systemProperties = systemProperties;
    this.testResults = new ArrayList<TestResult>();
  }

  /**
   * Adds a TestResult object to this TestReport.
   *
   * @param result the TestResult object to be added
   */
  public void addTestResult(TestResult result)
  {
    this.testResults.add(result);
  }

  /**
   * Writes a representation of this TestReport object in xml format.
   *
   * @param f the file where the xml stream gets written
   */
  public void writeXml(File f) throws IOException
  {
    Writer out = new OutputStreamWriter(new FileOutputStream(f), ENCODING);
    out.write("<?xml version='1.0' encoding='" + ENCODING + "'?>\n");
    out.write("<testreport version='0.1'>\n  <jvm name='"
              + escAttrib(systemProperties.get("java.vm.vendor"))
              + "'\n    version='"
              + escAttrib(systemProperties.get("java.vm.version")) + "' \n"
              + "    os='" + escAttrib(systemProperties.get("os.name")) + " "
              + escAttrib(systemProperties.get("os.version")) + " "
              + escAttrib(systemProperties.get("os.arch")) + "' />\n");
    Collections.sort(testResults);
    Iterator<TestResult> results = testResults.iterator();
    while (results.hasNext())
      {
        // Send a message to the Harness to let it know that we are
        // still writing the XML file.
        System.out.println("RunnerProcess:restart-timer");

        TestResult tr = results.next();
        String[] failures = tr.getFailMessags();
        String[] passes = tr.getPassMessages();
        out.write("  <testresult testlet='" + escAttrib(tr.getTestletName()));
        if (failures.length > 0 || passes.length > 0
            || tr.getException() != null)
          out.write("'>\n");
        else
          out.write("'/>\n");

        for (int i = 0; i < failures.length; i++) {
          // Restart timer.
          System.out.println("RunnerProcess:restart-timer");
          out.write("    <failure>" + esc(failures[i]) + "</failure>\n");
        }

        if (tr.getException() != null)
          {
            Throwable t = tr.getException();
            out.write("    <failure>\n      <exception class='"
                      + escAttrib(t.getClass().getName())
                      + "'>\n        <reason>" + esc(tr.getExceptionMessage())
                      + "</reason>\n        <message>\n" 
                      + esc(tr.getExceptionReason())
                      + "\n        </message>\n      </exception>" 
                      + "\n    </failure>\n");
          }

        for (int i = 0; i < passes.length; i++) {
          // Restart timer.
          System.out.println("RunnerProcess:restart-timer");
          out.write("    <pass>" + esc(passes[i]) + "</pass>\n");
        }

        if (failures.length > 0 || passes.length > 0
            || tr.getException() != null)
          out.write("  </testresult>\n");
      }
    out.write("</testreport>\n");
    out.close();
  }

  /**
   * Escapes chars &lt; &gt; and &amp; in str so that the result is
   * suitable for inclusion in an xml stream.
   */
  private String esc(String str)
  {
    if (str == null)
      return null;
    str = str.replaceAll("&", "&amp;");
    str = str.replaceAll("<", "&lt;");
    str = str.replaceAll(">", "&gt;");
    // This is a workaround for java.util.regex.Pattern.pcrematches.
    str = str.replace('', '?');
    return str;
  }

  /**
   * Escapes single quotes in string by prepending a backslash.
   */
  private String escAttrib(Object obj)
  {
    if (obj == null)
      return null;
    String str = (String)obj;
    str = str.replaceAll("'", "\\'");
    return str;
  }
}
