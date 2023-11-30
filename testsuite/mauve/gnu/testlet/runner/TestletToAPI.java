// Copyright (C) 2005 by <fduminy@jnode.org>
// Written by Fabien DUMINY (fduminy@jnode.org)

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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Generate 2 japi files that can be used to show the coverage 
 * (in term of methods, not of lines of code) of mauve testlets
 */
public class TestletToAPI
{
    private static final String TESTLET_SERIALIZATION = "serialization";
    private static final String TESTLET_CONSTRUCTORS = "constructors";
    private static final String TESTLET_CONSTANTS = "constants";
    private static final String TESTLET_CLASS_SUFFIX = "Class!"; // don't remove the "!" at the end !
    
    public static void main(String[] args) throws IOException
    {                
        if(args.length < 4)
        {
            usage();
            return;
        }
        
        final String inTestletsFile = args[0];
        final String inTestedFile = args[1];
        final String outTestletsFile = args[2];
        final String outTestedFile = args[3];
        
        // just insure that all files are differents
        // that's especially usefull to avoid erasing input files !
        for(int i = 0 ; i < 4 ; i++)
        {
            for(int j = 0 ; j < 4 ; j++)
            {
                if((j != i) && args[i].equals(args[j]))
                    throw new IllegalArgumentException("argument #"+i+" and argument #"+j+" are the same");
            }            
        }
        
        final Reader inTestlets = new FileReader(inTestletsFile);
        final Reader inTested = new FileReader(inTestedFile);
        final Writer outTestlets = new FileWriter(outTestletsFile);
        final Writer outTested = new FileWriter(outTestedFile);
        convert(inTestlets, inTested, outTestlets, outTested);
    }

    public static void usage()
    {
        System.out.println("usage: TestletToAPI inTestlets.japi inTested.japi outTestlets.japi outTested.japi");
        System.out.println("  inTestlets.japi: the output of japize on mauve testlets");
        System.out.println("  inTested.japi: the output of japize on tested api (Classpath, jdk, ...)");
        System.out.println("  outTestlets.japi, outTested.japi: 2 japi files to be used as input of japicompat");
    }
    
    public static void convert(Reader inTestlets, Reader inTested, Writer outTestlets, Writer outTested) throws IOException
    {        
        final Stats stats = new Stats();
        final Map methodToFullLine = processTestedAPI(inTested, outTested, stats);
        processMauveAPI(inTestlets, outTestlets, methodToFullLine, stats);
        
        System.out.println("=== Statistics ===");
        System.out.println("NbTestlets:"+stats.getNbTestlets());
        System.out.println("NbMethods:"+stats.getNbMethods());
        System.out.println("NbMethodsNotFound:"+stats.getNbMethodsNotFound());
        
        if(stats.getNbMethods() == 0)
            System.out.println("Estimated coverage : #ERROR#");
        else
        {
            double ratio = (double) stats.getNbTestlets() / (double) stats.getNbMethods();
            double percent = ((int)(10000 * ratio)) / 100.0d;
            System.out.println("Estimated coverage : "+percent+" %");
        }
    }
    
    public static void processMauveAPI(Reader in, Writer out, Map methodToFullLine,
            Stats stats) throws IOException
    {
        BufferedReader reader = null;
        BufferedWriter writer = null;
        try
        {
            reader = new BufferedReader(in);
            writer = new BufferedWriter(out);
            String l;
            StringBuffer line = new StringBuffer(128);
            Set testedMethods = new HashSet();
            boolean firstLine = true;
            while((l = reader.readLine()) != null)
            {
                line.setLength(0); // clear the buffer
                line.append(l);

                if(firstLine)
                {
                    writer.write(l);
                    writer.write('\n');
                    firstLine = false;
                    continue;
                }                
                
                if(!acceptAPILine(line.toString())) continue;
                
                Line mauveAPI = parseMauveAPILine(line);
                
                final String methodName = mauveAPI.getMethodName();
                if(testedMethods.contains(methodName)) continue;
                testedMethods.add(methodName);                
                
                Line testedAPI = (Line) methodToFullLine.get(methodName);
                if((testedAPI == null) || (testedAPI == Line.NULL))
                {
                    System.err.println("method not found in API : "+methodName);
                    stats.incNbMethodsNotFound();
                    continue;
                }
                
                String className = testedAPI.getClassName();
                if(!testedMethods.contains(className))
                {
                    // first method of the class, add the class declaration line
                    testedMethods.add(className);
                    
                    Line classDecl =  (Line) methodToFullLine.get(className);
                    if((classDecl == null) || (classDecl == Line.NULL))
                        throw new IllegalStateException(className + " has no declaration");
                    
                    writer.write(classDecl.getContent());                            
                    writer.write('\n');
                }
                
                // write the method (or constructors, or constants, or ...) declaration
                writer.write(testedAPI.getContent());                            
                writer.write('\n');
                stats.inNbTestlets();
            }
        }
        finally
        {
            if(reader != null)
            {
                reader.close();
            }
            if(writer != null)
            {
                writer.close();
            }
            
            in.close();
            out.close();
        }
    }
    
    public static Map processTestedAPI(Reader inTested, Writer outTested,
                Stats stats) throws IOException
    {
        Map methodToFullLine = new HashMap();
        BufferedReader reader = null;
        BufferedWriter writer = null;
        try
        {
            reader = new BufferedReader(inTested);
            writer = new BufferedWriter(outTested);
            String l;
            StringBuffer line = new StringBuffer(128); 
            boolean firstLine = true;
            while((l = reader.readLine()) != null)
            {
                line.setLength(0); // clear the buffer
                line.append(l);

                if(firstLine)
                {
                    firstLine = false;
                }
                else if(acceptAPILine(l))
                {
                    Line parsedLine = parseTestedAPILine(line);
                    String methodName = parsedLine.getMethodName();
                    
                    switch(parsedLine.getType())
                    {
                    case Line.CONSTANTS:
                    case Line.CONSTRUCTORS:
                        Line constLine = (Line) methodToFullLine.get(methodName);  
                        if((constLine == Line.NULL) || (constLine == null))
                        {
                            methodToFullLine.put(methodName, parsedLine);
                        }
                        else
                        {
                            constLine.append(parsedLine.getContent());
                        }
                        break;
                        
                    case Line.CLASS_DECL:
                        if(!methodToFullLine.containsKey(methodName))
                        {
                            methodToFullLine.put(parsedLine.getClassName(), parsedLine);
                            
                            if(parsedLine.isSerializable())
                            {
                                // this a serializable class, add a pseudo method
                                String serialMethod = parsedLine.getClassName()+'!'+TESTLET_SERIALIZATION;
                                String serialLine = serialMethod + "() Pcifu V";
                                Line sl = new Line(Line.SERIALIZATION, serialMethod, serialLine, true);
                                methodToFullLine.put(serialMethod, sl);
                                
                                // will write 2 lines on output :
                                // 1 for the declaration and 1 for the pseudo method
                                l += "\n" ;
                                if(l.charAt(0) == '+')
                                {
                                    if(l.charAt(1) == '+')
                                        l += "++";
                                    else
                                        l += "+";
                                }
                                l += serialLine;
                                stats.incNbMethods(); // 1 extra method (a pseudo method)
                            }
                        }
                        break;
                      
                    case Line.SERIALIZATION:
                        throw new IllegalStateException("type SERIALIZATION unexpected here");
                        
                    case Line.METHOD:
                        if(!methodToFullLine.containsKey(methodName))
                        {
                            methodToFullLine.put(methodName, parsedLine);
                        }
                        break;

                    default:
                        if(parsedLine != Line.NULL)
                        {
                            throw new IllegalStateException("unknown type of line");
                        }
                    } // switch
                } // if
                
                writer.write(l);
                writer.write('\n');
                stats.incNbMethods();
            } // while
        }
        finally
        {
            if(reader != null)
            {
                reader.close();
            }
            inTested.close();
            
            if(writer != null)
            {
                writer.close();
            }
            outTested.close();
        }
        
        return methodToFullLine;
    }
        
    public static Line parseTestedAPILine(StringBuffer japizeLine)
    {
        final String line = japizeLine.toString();
        boolean isClassDecl = preprocessAPILine(japizeLine);
        boolean isSerialClassDecl = false;
        // tested API (Classpath, jdk or something compatible)

        if(!isClassDecl && (japizeLine.indexOf("(") < 0))
        {
            // class declared as Serializable ?
            isSerialClassDecl = (japizeLine.indexOf("class#") >= 0) && 
                                        (japizeLine.lastIndexOf("java.io.Serializable") >= 0);
            
            boolean isNonSerialClassDecl = (japizeLine.indexOf("class:") >= 0);            
            boolean isInterfaceDecl = (japizeLine.indexOf("interface") >= 0);
            isClassDecl = (isNonSerialClassDecl || isSerialClassDecl || isInterfaceDecl);
        }
        if(isClassDecl)
        {
            // special case for class declaration (Serializable or not) and
            // interface declarations
            int idx = japizeLine.indexOf("!");
            if(idx < 0) return Line.NULL;
          
            japizeLine.setLength(idx+1);            
            return new Line(Line.CLASS_DECL, japizeLine.toString(), line, isSerialClassDecl);            
        }
        
        int idx = japizeLine.indexOf("(");
        if(idx < 0)
        {
            // constant ?
            idx = japizeLine.indexOf("!#");
            if(idx < 0) return Line.NULL;
                
            // special case for constants
            japizeLine.setLength(idx+1);
            japizeLine.append(TESTLET_CONSTANTS);
            return new Line(Line.CONSTANTS, japizeLine.toString(), line);                
        }
        
        boolean isConstructor = (japizeLine.indexOf("constructor") >= 0);
        if(isConstructor)
        {
            // special case for constructors
            japizeLine.setLength(idx);                    
            japizeLine.append(TESTLET_CONSTRUCTORS);
            return new Line(Line.CONSTRUCTORS, japizeLine.toString(), line);                
        }
        
        if(idx >= 0)
        {
            // simple method
            japizeLine.setLength(idx);                    
            return new Line(Line.METHOD, japizeLine.toString(), line);
        }
        
        return Line.NULL;
    }

    public static Line parseMauveAPILine(StringBuffer japizeLine)
    {
        int type;
        final String line = japizeLine.toString();        
        preprocessAPILine(japizeLine);
        
        // mauve test API

        japizeLine.delete(0, "gnu.testlet.".length());
        
        int idx = japizeLine.indexOf("!");
        if(idx < 0) return Line.NULL;
        japizeLine.setLength(idx);
        
        idx = japizeLine.indexOf(",");
        if(idx < 0) return Line.NULL;
        japizeLine.setCharAt(idx, '!');
        
        idx = japizeLine.lastIndexOf("."); 
        if(idx < 0) return Line.NULL;
        japizeLine.setCharAt(idx, ',');
        
        // special case for some classes 
        // (due to case conflicts in filenames under Windows)
        // example : ColorClass->Color, ...
        idx = japizeLine.indexOf(TESTLET_CLASS_SUFFIX);
        if(idx >= 0)
        {
            japizeLine.delete(idx, idx+TESTLET_CLASS_SUFFIX.length()-1);
        }
        
        if(japizeLine.indexOf(TESTLET_CONSTANTS) >= 0)
            type = Line.CONSTANTS;
        else if(japizeLine.indexOf(TESTLET_CONSTRUCTORS) >= 0)
            type = Line.CONSTRUCTORS;
        else if(japizeLine.indexOf(TESTLET_SERIALIZATION) >= 0)
            type = Line.SERIALIZATION;
        else
            type = Line.METHOD;
                
        return new Line(type, japizeLine.toString(), line);
    }
    
    public static boolean acceptAPILine(String line)
    {
//        final String filter = "java.awt.AWTKeyStroke";
//        return line.startsWith("gnu.testlet."+filter) ||
//               line.startsWith(filter);
        
        boolean isMauveFramework = line.toString().startsWith("gnu.testlet,");        
//        return !isMauveFramework && 
//               (line.contains("java.awt.BasicStroke") ||
//                line.contains("java.awt,BasicStroke") );
        return !isMauveFramework;
    }
    
    public static class Stats
    {
        private int nbTestlets;
        private int nbMethods; // include special methods (serialization, constructors, constants, ...)
        private int nbMethodsNotFound;
        
        public Stats()
        {
            this.nbTestlets = 0;
            this.nbMethods = 0;
            this.nbMethodsNotFound = 0;
        }

        /**
         * @return Returns the nbMethods.
         */
        final int getNbMethods()
        {
            return nbMethods;
        }

        final void incNbMethods()
        {
            this.nbMethods++;
        }

        /**
         * @return Returns the nbTestlets.
         */
        final int getNbTestlets()
        {
            return nbTestlets;
        }

        final void inNbTestlets()
        {
            this.nbTestlets++;
        }

        /**
         * @return Returns the nbMethodsNotFound.
         */
        final int getNbMethodsNotFound()
        {
            return nbMethodsNotFound;
        }

        /**
         * @param nbMethodsNotFound The nbMethodsNotFound to set.
         */
        final void incNbMethodsNotFound()
        {
            this.nbMethodsNotFound++;
        }
        

    }
    
    public static class Line
    {
        public static final Line NULL = new Line(); 
        
        private static final int METHOD        = 0;
        private static final int CONSTRUCTORS  = 1;
        private static final int CONSTANTS     = 2;
        private static final int SERIALIZATION = 3;
        private static final int CLASS_DECL    = 4;
        
        final private int type;
        final private String className;
        final private String methodName; // in fact: classname + '!' + methodName
        final private StringBuffer lines;
        final private boolean serializable;

        public Line(int type, String methodName, String line)
        {
            this(type, methodName, line, false);
        }
        
        public Line(int type, String methodName, String line, boolean serializable)
        {
            this.type = type;
            this.methodName = methodName;
            this.lines = new StringBuffer(line);
            this.serializable = serializable;
            
            int idx = methodName.indexOf('!');
            if(idx < 0) throw new IllegalArgumentException("methodName must contains the '!' character");                
            this.className = methodName.substring(0, idx);
        }
        
        public int getType()
        {
            return type;
        }
        
        public boolean isSerializable()
        {
            return serializable;
        }

        public String getContent()
        {
            return lines.toString();
        }

        private Line()
        {
            this.type = -1;
            this.className = "";
            this.methodName = "";
            this.lines = null;      
            this.serializable = false;
        }
        
        public void append(String nextLine)
        {
            if(!isConstructor() && !isConstant()) 
                throw new UnsupportedOperationException("reserved to constructor and constant lines");
            
            lines.append('\n');
            lines.append(nextLine);
        }
        
        public boolean isConstructor()
        {
            return type == CONSTRUCTORS;
        }
        
        public boolean isConstant()
        {
            return type == CONSTANTS;
        }

        /**
         * @return Returns the methodName.
         */
        final public String getMethodName()
        {
            return methodName;
        }
        
        public String getClassName()
        {
            return className;
        }
        
        public String toString()
        {
            return "["+methodName+"]->"+lines.toString();
        }
    }

    public static boolean preprocessAPILine(StringBuffer japizeLine)
    {
        boolean isClassDecl = false;
        if(japizeLine.charAt(0) == '+')
        {
            if(japizeLine.charAt(1) == '+')
            {
                japizeLine.delete(0, 2);
                
                // declaration of java.lang.Object ?
                isClassDecl = (japizeLine.toString().endsWith("class"));
            }
            else
            {
                japizeLine.delete(0, 1);
            }
        }
        
        return isClassDecl;
    }
}
