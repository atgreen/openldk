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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.util.List;

/**
 * Abstract class for writing a {@link Comparison}
 * 
 * @author fabien
 *
 */
public abstract class ComparisonWriter {
    
    /**
     * Write the given comparison
     * 
     * @param comp
     * @param output
     * @throws FileNotFoundException
     */
    public final void write(RunComparison comp, File output) throws FileNotFoundException {
        PrintWriter ps = null;

        try {
            ps = new PrintWriter(new FileOutputStream(output));
            write(comp, ps);
        } finally {
            if (ps != null) {
                ps.close();
            }
        }
    }
    
    /**
     * Write the given comparison
     * 
     * @param run
     * @param pw
     */
    public final void write(RunComparison run, PrintWriter pw) {
        final Visitor v = createVisitor(pw);
        
        v.writeBegin();
        v.writeSummary(run);
        
        write(run, pw, v, EvolutionType.REGRESSION, "Regressions");
        write(run, pw, v, EvolutionType.PROGRESSION, "Progressions");
        write(run, pw, v, EvolutionType.STAGNATION, "Stagnations");
        
        v.writeEnd();
        
        pw.flush();
    }

    private void write(RunComparison run, PrintWriter pw, Visitor v, EvolutionType type, String typeLabel) {
        v.setType(type);
        v.setEvolutionLabel(typeLabel);
        
        v.writeBeginTable();
        run.accept(v);
        v.writeEndTable();
    }
    
    protected abstract Visitor createVisitor(PrintWriter pw);
    
    protected static class Level {
        public static final Level RUN = new Level(0);
        public static final Level PACKAGE = new Level(1);
        public static final Level CLASS = new Level(2);
        public static final Level TEST = new Level(3);
        
        private static final Level[] VALUES = {RUN, PACKAGE, CLASS, TEST};
        
        public static final Level[] values() {
            return VALUES;
        }

        public static final Level MAX = VALUES[VALUES.length - 1];
        
        private final int level;
        
        private Level(final int level) {
            this.level = level;
        }
        
        public int getValue() {
            return level;
        }
    }

    protected abstract static class Visitor implements ComparisonVisitor { 
        protected final PrintWriter pw;
        protected EvolutionType type; 
        protected String evolutionLabel;
        
        protected Visitor(PrintWriter pw) {
            this.pw = pw;
        }
        
        public void setType(EvolutionType type) {
            this.type = type;
        }
        
        public void setEvolutionLabel(String label) {
            this.evolutionLabel = label;
        }

        protected abstract void writeSummary(int nbRegressions, int nbProgressions, int nbStagnations);
        protected abstract void writeSystemProperties(List systemProperties, String result1Name, String result2Name);
        
        private void writeSummary(RunComparison run) {
            EvolutionTypeVisitor evolTypeVisitor = new EvolutionTypeVisitor();
            run.accept(evolTypeVisitor);
            
            writeSummary(evolTypeVisitor.getCounter(EvolutionType.REGRESSION), 
                    evolTypeVisitor.getCounter(EvolutionType.PROGRESSION),
                    evolTypeVisitor.getCounter(EvolutionType.STAGNATION));
            writeSystemProperties(run.getSystemProperties(), run.getResult1Name(), run.getResult2Name());
        }
        
        public void writeBegin() {
        }

        public void writeEnd() {
        }

        public void writeBeginTable() {
        }

        public void writeEndTable() {
        }
        
        //@Override
        public final void visit(RunComparison run) {
            if (shouldWrite(run)) {
                write(Level.RUN, run, true);
            }
        }
    
        //@Override
        public final void visit(PackageComparison pkg) {
            if (shouldWrite(pkg)) {
                write(Level.PACKAGE, pkg, true);
            }
        }
    
        //@Override
        public final void visit(ClassComparison cls) {
            if (shouldWrite(cls)) {
                write(Level.CLASS, cls, true);
            }
        }
    
        //@Override
        public final void visit(TestComparison test) {
            if (shouldWrite(test)) {
                write(Level.TEST, test, false);
                
                CheckResult cr = test.getCheckResult();
                String result;
                if (cr == null) {
                    result = "<no checkpoint>";
                } else {
                    result = Integer.toString(cr.getNumber()) + ':';
                    
                    if (cr.getCheckPoint() == null) {
                        result += "<no name>";
                    } else {
                        result += cr.getCheckPoint();
                    }
                }
                writeCheckResult(result);
                
                writeEndLine();
            }
        }

        protected abstract void writeBeginLine(Level level);
        protected abstract void writeName(Level level, String name);
        protected abstract void writeEndLine();
        
        protected abstract void writeCheckResult(String result);

        protected final void writeIndent(Level level) {
            final int indent = level.getValue() * 4;
            for (int i = 0; i < indent; i++) {
                pw.append(' ');
            }
        }
        
        private boolean shouldWrite(Comparison comp) {
            EvolutionTypeVisitor v = new EvolutionTypeVisitor();
            comp.accept(v);
            return (v.getCounter(type) > 0);
        }
        
        private void write(Level level, Comparison comp, boolean endLine) {
            writeBeginLine(level);
            
            writeName(level, comp.getName());
            
            if (endLine) {
                writeEndLine();
            }
        }
    };
}
