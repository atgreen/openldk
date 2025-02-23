// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.io.*;

class Test {
    public static void main(String[] args) throws Exception {
        foo();
        System.out.println(0);
        System.exit(0);
    }
    static void foo() throws Exception {
        try {
            bar();
        } catch (Exception e) {
            System.out.println(0);
            System.exit(0);
        }
        System.out.println(0);
        System.exit(0);
    }
    static void bar() throws Exception {
        try {
            try {
                throw new Exception();
            } catch (IOException ie) {
                System.out.println(0);
                System.exit(0);
            }
        } catch (Exception e) {
            System.out.println(1);
            System.exit(1);
        }
    }
}
