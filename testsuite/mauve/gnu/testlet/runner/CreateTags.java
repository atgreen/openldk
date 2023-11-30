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

public class CreateTags {
    private File base, output;
    private List classes = new ArrayList();

    public CreateTags(File base, File output) {
        if(!base.exists())
            throw new IllegalArgumentException("base dir does not exist");
        if(!base.isDirectory())
            throw new IllegalArgumentException("base dir is not a directory");
        if(output.exists())
            output.delete();
        this.base = base;
        this.output = output;
    }

    public void create() throws IOException {
        append(base);

        FileWriter writer = new FileWriter(output);

        // JDK
        String start = "1.0";
        String next;
        boolean first;
        do {
            first = true;
            next = "9.9";
            Iterator iter = classes.iterator();
            while(iter.hasNext()) {
                TestCase testCase = (TestCase) iter.next();
                int from = testCase.tags.fromJDK.compareTo(start);
                if(from < 0) {
                    if(testCase.tags.toJDK.compareTo(start) == 0) { // write tests not to be used
                        if(first) {
                            writer.write("[JDK"+ start +"]\n");
                            first = false;
                        }
                        writer.write("-");
                        writer.write(testCase.className);
                        writer.write("\n");
                    }
                }
                else if(from == 0) {
                    if(first) {
                        writer.write("[JDK"+ start +"]\n");
                        first = false;
                    }
                    writer.write(testCase.className);
                    writer.write("\n");
                }
                else if(from > 0) { // can't use, too old.
                    if(testCase.tags.fromJDK.compareTo(next) < 0)
                        next = testCase.tags.fromJDK;
                }
            }
            start = next;
            writer.write("\n");
        } while(!first);

        first=true;
        Iterator iter = classes.iterator();
        while(iter.hasNext()) {
            TestCase testCase = (TestCase) iter.next();
            if(testCase.tags.gui) {
                if(first) {
                    writer.write("\n[GUI]\n");
                    first = false;
                }
                writer.write(testCase.className);
                writer.write("\n");
            }
        }

        writer.close();
    }

    private void append(File dir) {
        File[] children = dir.listFiles();
        testCase: for(int i=0; i < children.length; i++) {
            File file = children[i];
            if(file.isDirectory()) {
                append(file);
                continue;
            }
            if(! file.getName().endsWith(".java"))
                continue;

            String tags = null, pckage = null;
            try {
                Reader reader = new FileReader(file);
                StringBuffer buf = new StringBuffer();
                int maxLines=30;
                line: while(maxLines > 0 && (tags == null || pckage == null)) {
                    int character = reader.read();
                    if(character == -1)
                        break;
                    if(character == '\n') {
                        int index = buf.indexOf("Tags:") + 5; // 5 == length of string
                        if(index > 5 && buf.length() > index) {
                            String line = buf.substring(index).trim().toLowerCase();
                            if("not-a-test".equals(line))
                                continue testCase;
                            if(tags != null)
                                tags += " "+ line;
                            else
                                tags = line;
                        }
                        else if(buf.indexOf("package ") == 0)
                        {
                            int idx = buf.lastIndexOf(";");
                            pckage = buf.substring(8, idx);
                        }
                        buf = new StringBuffer();
                        maxLines--;
                    }
                    else
                        buf.append((char) character);
                }
                if(pckage != null && tags != null) {
                    String className = pckage +"."+ file.getName().substring(0,
                            file.getName().length()-5);
                    classes.add(new TestCase(className, tags));
                }
            } catch(IOException e) {
                e.printStackTrace(); // TODO nicer??
            }
        }
    }

    public static void main(String[] args) throws IOException {
        new CreateTags(new File(args[0]), new File(args[1])).create();
    }

    private static class TestCase {
        public final String className;
        public final Tags tags;
        public TestCase(String className, String tags) {
            this.className = className;
            this.tags = new Tags(tags);
        }
    }
}
