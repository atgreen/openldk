// Tags: not-a-test
/*
Copyright (c) 2004, 2005 Thomas Zander <zander@kde.org>

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

class Tags {
    boolean gui = false;
    String fromJDK="1.0", toJDK="99.0";
    String fromJDBC="1.0", toJDBC="99.0";
    public Tags(String line) {
        int start=0;
        for(int i=0; i <= line.length();i++) {
            if(i == line.length() || line.charAt(i) == ' ') {
                if(start < i)
                    process(line.substring(start, i));
                start = i+1;
            }
        }

    }
    public void process(String token) {
//System.out.println("     +-- '"+ token +"'");
        boolean end = token.startsWith("!");
        if(end)
            token = token.substring(1);
        if(token.startsWith("jls") || token.startsWith("jdk")) {
            String value = token.substring(3);
            if(end)
                toJDK = value;
            else
                fromJDK = value;
        }
        else if(token.startsWith("jdbc")) {
            String value = token.substring(4);
            if(end)
                toJDBC = value;
            else
                fromJDBC = value;
        }
        else if(token.startsWith("gui"))
            gui = true;
    }
    public boolean isValid(double javaVersion, double JDBCVersion) throws NumberFormatException {
        if(javaVersion != 0d) {
            double from = Double.parseDouble(fromJDK);
            if(from > javaVersion)  return false;
            double end = Double.parseDouble(toJDK);
            if(end < javaVersion)  return false;
        }
        if(JDBCVersion != 0d) {
            double from = Double.parseDouble(fromJDBC);
            if(from < JDBCVersion)  return false;
            double end = Double.parseDouble(toJDBC);
            if(end > JDBCVersion)  return false;
        }
        return true;
    }
}
