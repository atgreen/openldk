// Tags: not-a-test
// Copyright (C) 2004 by Object Refinery Limited
// Written by David Gilbert (david.gilbert@object-refinery.com)
// Modified by Fabien DUMINY (fduminy@jnode.org)

// This file is part of Mauve Reporter.

// Mauve Reporter is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.

// Mauve Reporter is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve Reporter; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
package gnu.testlet.runner;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 * Represents the result of running a collection of Mauve tests.
 */
public class RunResult implements Result {

    /** The name of the run. */
    private String name;

    /** A list containing results for each class in the package. */
    private List packageResults;

    /** A list containing unfound test-names */
    private List missingTests = new ArrayList();
    private List faultyTests = new ArrayList();
    private boolean sorted=true;
    
    /**
     * System properties in which mauve tests are run.
     * The list is actually of list of couples(name, value), which make it a
     * kind of order Map.
     * Example of list content : "name1", "value1", "name2", "value2" 
     */
    private final List systemProperties;

    /**
     * Creates a new result, initially empty.
     *
     * @param name  the class name.
     */
    public RunResult(String name) {
        this.name = name;
        packageResults = new ArrayList();
        systemProperties = new ArrayList();
    }

    /**
     * Set a system property.
     * @param name Name of the system property.
     * @param value Value of the system property.
     */
    public void setSystemProperty(String name, String value) {
        boolean found = false;
        for (int i = 0; i < systemProperties.size(); i += 2) {
            if (name.equals((String) systemProperties.get(i))) {
                systemProperties.set(i + 1, value);
                found = true;
                break;
            }
        }
        
        if (!found) {
            systemProperties.add(name);
            systemProperties.add(value);
        }
    }

    /**
     * Get a system property.
     * @param name Name of the system property.
     * @return value Value of the system property.
     */
    public String getSystemProperty(String name) {
        String value = "";
        for (int i = 0; i < systemProperties.size(); i += 2) {
            if (name.equals((String) systemProperties.get(i))) {
                value = (String) systemProperties.get(i + 1);
                break;
            }
        }
        return value;
    }

    /**
     * Get a system property names.
     * @return array of system property names.
     */
    public String[] getSystemPropertyNames() {
        String[] names = new String[systemProperties.size() / 2];
        int j = 0;
        for (int i = 0; i < systemProperties.size(); i += 2) {
            names[j++] = (String) systemProperties.get(i);
        }
        return names;
    }

    /**
     * Returns the run name.
     *
     * @return The run name.
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the run name.
     *
     * @param name  the name.
     */
    void setName(String name) {
        this.name = name;
    }

    /**
     * Adds a package result.
     *
     * @param result  the package result.
     */
    public void add(PackageResult result) {
        packageResults.add(result);
        sorted =false;
    }

    /**
     * Returns an iterator that provides access to the package results.
     *
     * @return An iterator.
     */
    public Iterator getPackageIterator() {
        sortPackages();
        return packageResults.iterator();
    }

    public Iterator getMissingTestsIterator() {
        return missingTests.iterator();
    }

    public Iterator getFaultyTestsIterator() {
        return faultyTests.iterator();
    }

    /**
     * Returns the total number of checks performed for this run.
     *
     * @return The check count.
     */
    public int getCheckCount() {
        int result = 0;
        Iterator iterator = getPackageIterator();
        while (iterator.hasNext()) {
            PackageResult pr = (PackageResult) iterator.next();
            result = result + pr.getCheckCount();
        }
        return result;
    }

    /**
     * Returns the number of checks with the specified status.
     *
     * @param passed  the check status.
     *
     * @return The number of checks passed or failed.
     */
    public int getCheckCount(boolean passed) {
        int result = 0;
        Iterator iterator = getPackageIterator();
        while (iterator.hasNext()) {
            PackageResult pr = (PackageResult) iterator.next();
            result = result + pr.getCheckCount(passed);
        }
        return result;
    }

    /**
     * Returns the index of the specified result, or -1.
     *
     * @param pr  the package result.
     *
     * @return The index.
     */
    public int indexOf(PackageResult pr) {
        sortPackages();
        return packageResults.indexOf(pr);
    }

    /**
     * Returns the package result with the specified name.
     *
     * @param name  the package name.
     *
     * @return The package result, or null when not found.
     */
    public PackageResult getPackageResult(String name) {
        sortPackages();
        for (int i = 0; i < packageResults.size(); i++) {
            PackageResult rr = (PackageResult) packageResults.get(i);
            if (rr.getName().equals(name))
                return rr;
        }
        return null;
    }

    void addMissingTest(String line) {
        missingTests.add(line);
    }

    void addFaultyTest(String line, String failure) {
        faultyTests.add(new String[] {line, failure});
    }

    /**
     * Sorts the package results.
     */
    private void sortPackages() {
        if(sorted) return;
        Collections.sort(packageResults);
        sorted = true;
    }
}
