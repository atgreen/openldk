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

import java.util.List;
import java.util.ArrayList;

/**
 * A TestResult object represents the results a run of one Testlet. TestResult
 * objects are normally agregated in a TestReport. The natural ordering of
 * TestResult is defined to be the same as the natural order of their 
 * respective testletName field.
 */
public class TestResult
  implements Comparable<TestResult>
{
  private String testletName = null;
  private List<String> failMessages = new ArrayList<String>();
  private List<String> passMessages = new ArrayList<String>();
  private Throwable exception = null;
  private String exceptionReason = null;
  private String exceptionMessage = null;

  /**
   * Constructs a TestResult instance with testletName set to the given name.
   *
   * @param testletName the name of the testlet that produced this TestResult
   */
  public TestResult(String testletName)
  {
    if (testletName == null)
      throw new IllegalArgumentException("testletName can not be null");
    this.testletName = testletName;
  }

  /**
   * Adds a pass message identifying a passing test.  Should be called 
   * when a test passes.
   * 
   * @param message a String that identifies the test that passed
   */
  public void addPass(String message)
  {
    passMessages.add(message);
  }

  /**
   * Adds a failure message identifying a failing test. Should be called when
   * a test fails. 
   *
   * @param message a String that identifies the test that failed inside
   * this servlet
   */
  public void addFail(String message)
  {
    failMessages.add(message);
  }

  /**
   * Adds an Exception and optional identification message to this TestResult
   * object. Should be called when the instantiation or execution of a Testlet
   * results in an exception.
   *
   * @param exception The exception that was thrown
   * @param message A message that identifies the test that caused the
   * @param reason the stack trace for the Exception
   * exception to be thrown
   */
  public void addException(Throwable exception, String message, String reason)
  {
    if (this.exception != null)
      throw new IllegalArgumentException("trying to add more than one " +
               "exception to TestResult");           
    this.exception = exception;
    this.exceptionMessage = message;
    this.exceptionReason = reason;
  }

  /**
   * The number of tests that have preformed without failure or exceptions.
   */
  public int getPassCount()
  {
    return passMessages.size();
  }

  /**
   * An array of Strings that holds the identifying messages for all failed
   * tests.
   */
  public String[] getFailMessags()
  {
    return failMessages.toArray(new String[failMessages.size()]);
  }

  /**
   * An array of Strings that holds the identifying messages for all
   * passing tests.
   * 
   * @return an array of Strings holding the messages for passing tests.
   */
  public String[] getPassMessages()
  {
    return passMessages.toArray(new String[passMessages.size()]);
  }
  
  /**
   * The name of the Testlet that this TestResult holds information about.
   */
  public String getTestletName()
  {
    return testletName;
  }

  /**
   * If an Exception was thrown when the Testlet was instantiated or run it
   * is returned, else null is returned.
   */
  public Throwable getException()
  {
    return exception;
  }

  /**
   * If an Exception was thrown when the Testlet was instantiated or run,
   * this String identifies what test (or other contition) caused the test.
   */
  public String getExceptionMessage()
  {
    return exceptionMessage;
  }
  
  /**
   * If an Exception was thrown when the Testlet was instantiated or run,
   * this String is the stack trace associated with the Exception.
   * 
   * @return the stack trace associated with the Exception
   */
  public String getExceptionReason()
  {
    return exceptionReason;
  }

  /**
   * Compares one TestResult object to another. TestResult objects compare
   * the same as their testletName fields.
   */ 
  public int compareTo(TestResult other)
  {
    return testletName.compareTo(other.testletName);
  }
}
