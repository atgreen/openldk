// $Id: pr391.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.io.*;
import java.lang.*;
import java.util.*;

/**
 * TODO
 * 1. throw error for mouting a drive
 */

class inner {
	private StringBuffer[] execute(String args[]) {
		StringBuffer[] result = new StringBuffer[2];  // 0 - stdout, 1 -
		stdin
				int bufferSize        = 256;     // used to prevent deadlock in
		exec().

				try {
		// Run command
			Process p = Runtime.getRuntime().exec(args);

		// Thread for reading InputStreams to avoid deadlock on win32
			class ioThread implements Runnable {
				private BufferedReader bo = null;
				public StringBuffer result = null;
				private ioThread(InputStream i,int size) {
					bo = new BufferedReader(new InputStreamReader(i),size);
				}
				public void run() {
					String qo;
					result = new StringBuffer();
					try {
						while ((qo=bo.readLine()) != null)
							result.append(qo+"\n");
						bo.close();
					}
					catch (Exception e) {
						result = null;
					}
				}
			}

		// Get from Standard OUT/INT
			ioThread out = new ioThread(p.getInputStream(),bufferSize);
			ioThread err = new ioThread(p.getErrorStream(),bufferSize);
			new Thread(out).start();
			new Thread(err).start();

			p.waitFor();  // wait for original process to end;

			result[0] = out.result;
			result[1] = err.result;
		}
		catch (Exception e) {
			return null;
		}
		System.out.println("STDOUT:");
		System.out.println(result[0]);
		System.out.println("STDERR:");
		System.out.println(result[1]);
		return result;
	}

	// Testing purposes
	public static void main(String args[]) {
		inner n = new inner();
		n.execute(args);
	}
}

