// Copyright (c) 2008 Fabien DUMINY (fduminy@jnode.org)

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.runner;


/**
 * Constants for tags used by {@link XMLReportParser} and {@link XMLReportWriter}.
 *  
 * @author fabien
 *
 */
public interface XMLReportConstants {
    public static final String RUN_RESULT = "run";
    public static final String RUN_NAME = "name";
    
    public static final String PROPERTY = "property";
    public static final String PROPERTY_NAME = "name";
    public static final String PROPERTY_VALUE = "value";
    
    public static final String PACKAGE_RESULT = "package";
    public static final String PACKAGE_NAME = "name";
    
    public static final String CLASS_RESULT = "class";
    public static final String CLASS_NAME = "name";
    
    public static final String TEST_RESULT = "test";
    public static final String TEST_NAME = "name";
    public static final String TEST_ERROR = "error";
    
    public static final String CHECK_RESULT = "check";
    public static final String CHECK_NUMBER = "number";
    public static final String CHECK_POINT = "check-point";
    public static final String CHECK_PASSED = "passed";
    public static final String CHECK_EXPECTED = "expected";
    public static final String CHECK_ACTUAL = "actual";
    public static final String CHECK_LOG = "log";
}
