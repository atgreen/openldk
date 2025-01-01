// Copyright (C) 2004, 2005 by Object Refinery Limited
// Copyright (C) 2005 by <zander@kde.org>
// Written by David Gilbert (david.gilbert@object-refinery.com)
// Written by Thomas Zander <zander@kde.org>
// Modified by Fabien DUMINY (fduminy@jnode.org)

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.


package gnu.testlet.runner;

import gnu.testlet.ResourceNotFoundException;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.Locale;

/**
 * An application that runs a collection of Mauve tests and creates a report 
 * summarizing the results.
 */
public class Mauve extends TestHarness 
{
  protected String lastCheckPoint;
  protected int checksSinceLastCheckPoint;
  protected ClassResult classResult;
  protected TestResult currentTest;
  protected CheckResult currentCheck;
  protected RunResult result;
  
  /**
   * runs tests
   *
   * @param file  the text file containing test class names.
   * @param prefix  the prefix for each test class (usually 'gnu.testlet').
   * @param output  the name of the directory for writing output.
   */
  public synchronized void execute(String file, String prefix, String output) 
  {
    // save the default locale, some tests change the default and we want
    // to restore it before generating the HTML report...
    Locale savedLocale = Locale.getDefault();
    File out = new File(output);
    if (out.exists() && !out.isDirectory())
      throw new IllegalArgumentException("Output should be a directory");
    if (!out.exists())
      out.mkdirs();

    result = new RunResult("Mauve Test Run");
    addSystemProperties(result);
    currentCheck = new CheckResult(0, false);
    // initialize
    // run tests and collect results
    File f = new File(file);
    try 
      {
        FileReader testsToRun = new FileReader(f);
        LineNumberReader r = new LineNumberReader(testsToRun);
        while (r.ready()) 
          {
            String line = r.readLine();
            executeLine(prefix, line);
        }
    }
    catch (FileNotFoundException e) 
    {
      throw new IllegalArgumentException(e.getMessage());
    }
    catch (IOException e) {
      e.printStackTrace(System.out);
    }

    // tests are complete so restore the default locale
    Locale.setDefault(savedLocale);
    
    writeHTMLReport(out);
    writeXMLReport(out);
    System.out.println("DONE!");
  }
  
  /**
   * Add the most interesting system properties to describe the virtual machine
   * being tested. Developers using Mauve APIs can also add their own (specific)
   * properties by calling {@link RunResult#setSystemProperty(String, String)}.
   * @param runResult The RunResult to which add system properties.
   */
  protected void addSystemProperties(RunResult runResult) {
      runResult.setSystemProperty("java.version", System.getProperty("java.version"));
      runResult.setSystemProperty("java.vendor", System.getProperty("java.vendor"));
      runResult.setSystemProperty("java.vendor.url", System.getProperty("java.vendor.url"));
      runResult.setSystemProperty("os.name", System.getProperty("os.name"));
      runResult.setSystemProperty("os.arch", System.getProperty("os.arch"));
      runResult.setSystemProperty("os.version", System.getProperty("os.version"));

      runResult.setSystemProperty("java.vm.specification.version", System.getProperty("java.vm.specification.version"));
      runResult.setSystemProperty("java.vm.specification.vendor", System.getProperty("java.vm.specification.vendor"));
      runResult.setSystemProperty("java.vm.specification.name", System.getProperty("java.vm.specification.name"));
      runResult.setSystemProperty("java.vm.version", System.getProperty("java.vm.version"));
      runResult.setSystemProperty("java.vm.vendor", System.getProperty("java.vm.vendor"));
      runResult.setSystemProperty("java.vm.name", System.getProperty("java.vm.name"));
      runResult.setSystemProperty("java.specification.version", System.getProperty("java.specification.version"));
      runResult.setSystemProperty("java.specification.vendor", System.getProperty("java.specification.vendor"));
      runResult.setSystemProperty("java.specification.name", System.getProperty("java.specification.name"));
      runResult.setSystemProperty("java.class.version", System.getProperty("java.class.version"));
  }
  
  /**
   * Execute a single line of a stream, which might or might not be the name of
   * a mauve test (testlet).
   * @param prefix
   * @param line The line to execute.
   */
  protected void executeLine(String prefix, String line) {
        if ((line == null) || (line.trim().length() == 0)) { 
            return;
        }
  
        System.out.println(line);
        // check the line is not commented
        // load the listed class
        try 
          {
            Class c = Class.forName(line);
            // strip prefix ('gnu.testlet.') from front of name
            String temp = line.substring(prefix.length());
            // suffix is the name for the TestResult
            String testName = temp.substring(temp.lastIndexOf('.') + 1);

            temp = temp.substring(0, temp.lastIndexOf('.'));
            String className = temp.substring(temp.lastIndexOf('.') + 1);
            if (className.equals("Double") || className.equals("Float") 
                    || className.equals("Key")) 
              {
                if (!temp.startsWith("java.lang.")) 
                  {
                    temp = temp.substring(0, temp.lastIndexOf('.'));
                    className = temp.substring(temp.lastIndexOf('.') + 1) 
                        + '.' + className;
                  }
              }

            String packageName = "default package";
            int index = temp.lastIndexOf('.');
            if (index >= 0)
              packageName = temp.substring(0, temp.lastIndexOf('.'));
            // remaining suffix is name for ClassResult
            // rest of text is name for PackageResult
            PackageResult pr = result.getPackageResult(packageName);
            if (pr == null)
              pr = new PackageResult(packageName);
            classResult = pr.getClassResult(className);
            if (classResult == null)
              classResult = new ClassResult(className);

            Testlet testlet = createTestlet(c, line);
            if (testlet != null) {
                runTestlet(testlet, pr, testName);
            }
        } 
        catch (ClassNotFoundException e) 
        {
          System.err.println("Could not load test: "+ line);
          result.addMissingTest(line);
        }
  }
  
  /**
   * Execute a testlet.
   * @param testlet The {@link Testlet} to execute. 
   * @param pr The name of the {@link PackageResult} to which the testlet belong. 
   * @param testName The name used to describe the associated {@link TestResult}.
   */
  protected void runTestlet(Testlet testlet, PackageResult pr, String testName) {
      currentTest = new TestResult(testName);
      checksSinceLastCheckPoint = 0;
      lastCheckPoint = "-";
      try 
      {
        testlet.test(this);
      } 
      catch (Throwable t) 
      {
        t.printStackTrace(System.out);
        currentTest.failed(t);
      }
      classResult.add(currentTest);
      if (pr.indexOf(classResult) < 0)
        pr.add(classResult);
      if (result.indexOf(pr) == -1)
        result.add(pr);      
  }
  
  /**
   * Get the result of a mauve run.
   * @return The {@RunResult} describing the results of a mauve run.
   */
  protected RunResult getResult() {
      return result;
  }
  
  /**
   * Creates a new testlet instancen given by its class name.
   * @param c The class to instanciate, which should be a child class
   * of {@link Testlet}.
   * @param line The line giving the name of the testlet (used for error reporting).
   * @return an instance of the given testlet.
   */
  protected Testlet createTestlet(Class c, String line) {
      Testlet testlet = null;
      try 
      {
        testlet = (Testlet) c.newInstance();
      }
      catch (ClassCastException e) 
      {
        System.err.println("Not a test (does not implement Testlet): "
                + line);
        result.addFaultyTest(line, "Does not implement Testlet");
        // not a test
      } 
      catch (Throwable t) 
      { // instanciation errors etc..
        t.printStackTrace(System.out);
        result.addFaultyTest(line, t.getMessage());
      }
      return testlet;
  }

  /**
   * Write the results to an HTML report in the given output directory.
   * @param outputDir The output directory used to put the HTML report.
   */
  protected void writeHTMLReport(File outputDir) {
      // write results to HTML
      System.out.println("Creating HTML report...");
      try 
      {
        HTMLGenerator.createReport(result, outputDir);
      } 
      catch (IOException e) 
      {
        System.out.println("failed to write HTML due to following error:");
        e.printStackTrace(System.out);
      }      
  }
  
  /**
   * Write the results to an XML report in the given output directory.
   * @param outputDir The output directory used to put the XML report.
   */
  protected void writeXMLReport(File outputDir) {
      System.out.println("Creating XML report...");
      try 
      {
          File fx = new File(outputDir, "results.xml");
          new XMLReportWriter(true).write(result, fx);
          System.out.println("XML file written to " + fx.getAbsolutePath());
      } 
      catch (IOException e) 
      {
        System.out.println("failed to write XML due to following error:");
        e.printStackTrace(System.out);
      }
  }

  /**
   * Records the result of a boolean check.
   *
   * @param result  the result.
   */
  public void check(boolean result) 
  {
    currentCheck.setPassed(result);
    checkDone();
  }

  /**
   * Checks the two objects for equality and records the result of
   * the check.
   *
   * @param result  the actual result.
   * @param expected  the expected result.
   */
  public void check(Object result, Object expected) 
  {
    currentCheck.setPassed( (result != null) ? result.equals(expected) 
            : (expected == null));
    currentCheck.setActual((result != null) ? result.toString() : "null");
    currentCheck.setExpected((expected != null) ? expected.toString() : "null");
    checkDone();
  }

  /**
   * Checks two booleans for equality and records the result of the check.
   *
   * @param result  the actual result.
   * @param expected  the expected result.
   */
  public void check(boolean result, boolean expected) 
  {
    currentCheck.setPassed(result == expected);
    currentCheck.setActual(String.valueOf(result));
    currentCheck.setExpected(String.valueOf(expected));
    checkDone();
  }

  /**
   * Checks two ints for equality and records the result of the check.
   *
   * @param result  the actual result.
   * @param expected  the expected result.
   */
  public void check(int result, int expected) 
  {
    currentCheck.setPassed(result == expected);
    currentCheck.setActual(String.valueOf(result));
    currentCheck.setExpected(String.valueOf(expected));
    checkDone();
  }

  /**
   * Checks two longs for equality and records the result of the check.
   *
   * @param result  the actual result.
   * @param expected  the expected result.
   */
  public void check(long result, long expected) 
  {
    currentCheck.setPassed(result == expected);
    currentCheck.setActual(String.valueOf(result));
    currentCheck.setExpected(String.valueOf(expected));
    checkDone();
  }

  /**
   * Checks two doubles for equality and records the result of the check.
   *
   * @param result  the actual result.
   * @param expected  the expected result.
   */
  public void check(double result, double expected) 
  {
    currentCheck.setPassed((result == expected
                ? (result != 0) || (1/result == 1/expected)
                : (result != result) && (expected != expected)));
    currentCheck.setActual(String.valueOf(result));
    currentCheck.setExpected(String.valueOf(expected));
    checkDone();
  }

  /**
   * Records a check point.  This can be used to mark a known place in a
   * testlet.  It is useful if you have a large number of tests -- it makes
   * it easier to find a failing test in the source code.
   *
   * @param name  the check point name.
   */
  public void checkPoint(String name) 
  {
    lastCheckPoint = name;
    checksSinceLastCheckPoint = 0;
  }

  /**
   * Method called when a checkpoint has been reached.
   */
  private void checkDone() 
  {
    currentCheck.setNumber(++checksSinceLastCheckPoint);
    currentCheck.setCheckPoint(lastCheckPoint);
    currentTest.add(currentCheck);
    currentCheck = new CheckResult(0, false);
    currentCheck.setCheckPoint(lastCheckPoint);
  }

  /**
   * Writes a message to the debug log along with a newline.
   *
   * @param message  the message.
   */
  public void debug(String message) 
  {
    debug(message, true);
  }

  /**
   * Writes a message to the debug log with or without a newline.
   *
   * @param message  the message.
   * @param newline  a flag to control whether or not a newline is added.
   */
  public void debug(String message, boolean newline) 
  {
    currentCheck.appendToLog(message);
    if (newline)
      currentCheck.appendToLog("\n");
  }

  /**
   * Writes the contents of an array to the log.
   *
   * @param o  the array of objects.
   * @param desc  the description.
   */
  public void debug(Object[] o, String desc) 
  {
    StringBuffer logMessage = new StringBuffer();
    logMessage.append("Object array: ");
    logMessage.append(desc);
    if (o == null)
      logMessage.append("null");
    else
      expand(o, logMessage);
    currentCheck.appendToLog(logMessage.toString());
    currentCheck.appendToLog("\n");
  }

  /**
   * recursive helper method for debug(Object[], String)
   * @param array
   * @param buf
   */
  private void expand(Object[] array, StringBuffer buf) 
  {
    for (int i = 0; i < array.length; i++) 
    {
      buf.append("obj["+ i +"]: ");
      if (array[i] instanceof Object[])
        expand((Object[]) array[i], buf);
      else if (array[i] != null)
        buf.append(array[i].toString());
      else
        buf.append("null");
      if (i < array.length)
        buf.append(", ");
    }
  }

  /**
   * Writes a stack trace for the specified exception to the log for the 
   * current check.
   *
   * @param ex  the exception.
   */
  public void debug(Throwable ex) 
  {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    PrintWriter w = new PrintWriter(out, false);
    ex.printStackTrace(w);
    w.close();
    try 
    {
      out.close();
      debug(out.toString(), true);
    } 
    catch (IOException e) 
    { 
      /* this should never happen..*/
    }
  }

  /**
   * This will print a message when in verbose mode.
   *
   * @param message  the message.
   */
  public void verbose(String message) 
  {
    debug(message, true);
  }

  public Reader getResourceReader(String name) throws ResourceNotFoundException 
  {
    return new BufferedReader(new InputStreamReader(getResourceStream(name)));
  }

  public InputStream getResourceStream(String name) 
    throws ResourceNotFoundException 
  {
    // The following code assumes File.separator is a single character.
    if (File.separator.length() > 1)
      throw new Error ("File.separator length is greater than 1");
    String realName = name.replace('#', File.separator.charAt(0));
    try 
    {
      return new FileInputStream(getSourceDirectory() + File.separator 
              + realName);
    }
    catch (FileNotFoundException ex) {
      throw new ResourceNotFoundException(ex.getLocalizedMessage() +
                ": " + getSourceDirectory() + File.separator + realName);
    }
  }

  public String getSourceDirectory () 
  {
    return null; // TODO
  }

  public File getResourceFile (String name) throws ResourceNotFoundException 
  {
    // The following code assumes File.separator is a single character.
    if (File.separator.length () > 1)
      throw new Error ("File.separator length is greater than 1");
    String realName = name.replace ('#', File.separator.charAt (0));
    File f = new File(getSourceDirectory() + File.separator + realName);
    if (!f.exists()) 
    {
      throw new ResourceNotFoundException("cannot find mauve resource file" +
                ": " + getSourceDirectory() + File.separator + realName);
    }
    return f;
  }

  /**
   * Provide a directory name for writing temporary files.
   *
   * @return The temporary directory name.
   */
  public String getTempDirectory() 
  { 
    // TODO
    return "/tmp";
  }

  /**
   * Runs the application to generate an HTML report for a collection
   * of Mauve tests.
   *
   * @param args  the command line arguments.
   */
  public static void main(String[] args) 
  {
    // -prefix <package-prefix>
    // -output <root-directory-for-HTML-output>
    String file = "tests";
    String prefix = "gnu.testlet.";
    String output = "results";
    for (int i = 0; i < args.length; i++) 
      {
        String a = args[i];
        if (a.equals("--prefix") || a.equals("-p")) 
          {
            if (i < args.length) 
              {
                prefix = args[i+1];
                i++;
              }
            else 
              {
                System.err.println("prefix: value missing");
                return;
              }
          }
        else if (a.equals("--output") || a.equals("-o")) 
          {
            if (i < args.length) 
              {
                output = args[i+1];
                i++;
              }
            else 
              {
                System.err.println("output: value missing");
                return;
              }
          }
        else if (a.equals("--help") || a.equals("-h")) 
          {
            System.out.println("Usage:  Mauve [options] [inputfile]");
            System.out.println("reads test-class names from inputfile and executes them;");
            System.out.println("If no inputfile is passed, then tests.txt will be used");
            System.out.println(" options:");
            System.out.println("   --help    -h   this help");
            System.out.println("   --output  -o   the output directory [results]");
            System.out.println("   --prefix  -p   package prefix [gnu.testlet]");
            return;
          }
        else
          file = a;
      }
    try 
      {
        new Mauve().execute(file, prefix, output);
      } 
    catch (IllegalArgumentException e) 
      {
        System.err.println(e.getMessage());
        System.err.println("Try --help for more info");
      }
    System.exit(0);
  }
}
