/*
Copyright (c) 2005 Thomas Zander <zander@kde.org>

This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */
package gnu.testlet.runner;

import java.util.*;
import java.io.*;

public class Filter {
    public static void main(String[] args) throws IOException {
        System.out.println("Please specify which tests you want to run based on the JVM version");
        System.out.println("you want to test compatibility with.");
        System.out.println("After you select a file called 'tests' will be written and you then");
        System.out.println("have to type:");
        System.out.println(" export CLASSPATH=alltests.jar:$CLASSPATH");
        System.out.println(" myJava gnu.testlet.runner.Mauve tests");
        System.out.println("");
        //System.out.println("parsing classlist");

        final Vector options = new Vector();
        final TreeSet sorted = new TreeSet();
        
        readTestList(new LineProcessor() {
            public void processLine(StringBuffer buf) {        
                if(buf.indexOf("[") == 0 && buf.charAt(buf.length()-1) == ']') {
                    String token = buf.substring(1, buf.length()-1);
                    if(token.startsWith("JDK"))
                        sorted.add(token);
                    else
                        options.add(token);
                }
            }
        });
        
        //System.out.println("done");
        System.out.println("Please pick one:");
        final StringBuffer buf = new StringBuffer();
        int i=1;
        Iterator iter = sorted.iterator();
        while(iter.hasNext()) {
            buf.append(String.valueOf(i++));
            buf.append(": ");
            buf.append(iter.next().toString());
            if(iter.hasNext())
                buf.append(",  ");
            else
                buf.append("? ");
        }
        String answer = ask(buf.toString());
        final String which;
        try {
            int index = Integer.parseInt(answer);
            which = (String) new ArrayList(sorted).get(index-1);
        } catch(NumberFormatException e) {
            System.out.println("No parsable answer found");
            System.exit(-1);
            return;
        } catch(Exception e) {
            System.out.println("Sorry; I'm not sure I understand you, bailing out");
            System.exit(-1);
            return;
        }

        Iterator opsIter = new Vector(options).iterator();
        final List choosedOptions = new Vector();
        while(opsIter.hasNext()) {
            String option = (String) opsIter.next();
            answer = ask("Use classes with option: "+ option +"? [yN] ");
            if("y".equalsIgnoreCase(answer))
                choosedOptions.add(option);
        }

        System.out.println("Writing all tests for "+ which +" to 'tests'");

        final Set tests = new TreeSet();
        
        readTestList(new LineProcessor() {
            private boolean valid=true;
            
            public void processLine(StringBuffer buf) {
                if(buf.charAt(0) == '[') {
                    String newVer = buf.substring(1, buf.length()-1);
                    if(newVer.startsWith("JDK")) {
                        if(which.compareTo(newVer) < 0)
                            valid=false;
                    } else
                        valid= choosedOptions.contains(newVer);
                }
                else if(valid && buf.charAt(0) == '-')
                    tests.remove(buf.substring(1));
                else if (valid)
                    tests.add(buf.toString());
                else
                    tests.remove(buf.toString());
            }
        });
        
        PrintWriter writer = new PrintWriter(new FileOutputStream(new File("tests")));
        iter = tests.iterator();
        while(iter.hasNext())
            writer.println(iter.next().toString());
        writer.close();
    }

    public static interface LineProcessor {
        void processLine(StringBuffer buf);
    }

    public static void readTestList(LineProcessor processor) throws IOException {
        InputStream in = Filter.class.getResourceAsStream("/testslists");
        StringBuffer buf = new StringBuffer();
        while(true) {
            int character = in.read();
            if(character == -1)
                break;
            if(character == '\n') {
                if(buf.length() == 0)
                    continue;
                processor.processLine(buf);
                buf.setLength(0); // clear buffer
            }
            else
                buf.append((char) character);
        }
    }
    
    private static String ask(String question) throws IOException {
        System.out.write(question.getBytes());

        StringBuffer answer = new StringBuffer();
        while(true) {
            int ch = System.in.read();
            if (ch < 0 || ch == '\n')
                return answer.toString().trim();
            if (ch == '\r') continue;
            answer.append((char) ch);
        }
    }
}
