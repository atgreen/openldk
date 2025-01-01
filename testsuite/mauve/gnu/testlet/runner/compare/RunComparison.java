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

package gnu.testlet.runner.compare;

import gnu.testlet.runner.CheckResult;
import gnu.testlet.runner.ClassResult;
import gnu.testlet.runner.PackageResult;
import gnu.testlet.runner.RunResult;
import gnu.testlet.runner.TestResult;

import java.util.ArrayList;
import java.util.List;

/**
 * Result of the comparison of 2 {@link RunResult}s
 * @author fabien
 *
 */
public class RunComparison extends Comparison {
    private final List systemProperties = new ArrayList();
    private final String result1Name;
    private final String result2Name;
    
    RunComparison(RunResult result1, RunResult result2) {
        super(new RunResult("Comparison of '" + result1.getName() + "' and '" + result2.getName() + "'"));
        result1Name = result1.getName();
        result2Name = result2.getName();
    }

    /**
     * 
     * @param pkg
     * @param cls
     * @param test
     * @param check, might be null
     * @param nbProgressedCheck, might be < 0 for regressions in the test
     */
    public void setProgression(PackageResult pkg, ClassResult cls, TestResult test,
            CheckResult check, int nbProgressedChecks) {
        // package
        Comparison pc = get(pkg.getName());
        if (pc == null) {
            pc = new PackageComparison(pkg);
            add(pc);
        }

        // class
        Comparison classComp = pc.get(cls.getName());
        if (classComp == null) {
            classComp = new ClassComparison(cls);
            pc.add(classComp);
        }

        // test
        classComp.add(new TestComparison(test, check, nbProgressedChecks));
    }
    

    //@Override
    public void accept(ComparisonVisitor visitor) {
        visitor.visit(this);
        acceptChildren(visitor);
    }

    /**
     * @param name
     * @param value1
     * @param value2
     */
    public void addSystemProperty(String name, String value1, String value2) {
        systemProperties.add(name);
        systemProperties.add(value1);
        systemProperties.add(value2);
    }
    
    public List getSystemProperties() {
        return systemProperties;
    }

    public String getResult1Name() {
        return result1Name;
    }

    public String getResult2Name() {
        return result2Name;
    }
}
