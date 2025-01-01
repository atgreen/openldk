// Tags: JDK1.4

// Copyright (C) 2005 Mark J. Wielaard <mark@klomp.org>

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.nio.channels.FileChannel;

import java.io.*;
import java.nio.*;
import java.nio.channels.*;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Tests FileChannel lock(), tryLock() and FileLock methods.  We test
 * shared locking (which is optional for read only channels, so will
 * fail when not supported).
 */
public class lock implements Testlet, Runnable
{

  DataInputStream dis;
  DataOutputStream dos;
  BufferedReader br;

  TestHarness harness;

  public void test(TestHarness harness)
  {
    this.harness = harness;
    String filename = harness.getTempDirectory()
      + File.separator + "mauvelock";
    File file = new File(filename);

    FileOutputStream fos = null;
    FileInputStream fis = null;
    RandomAccessFile raf = null;
    FileChannel channel;

    try
      {
	// Setup. Remove and recreate test file
	// Fill the file with some stuff 5Mb in total
	// Start another runtime the check that locks actually work.
	file.delete();
	file.createNewFile();
	fos = new FileOutputStream(file);
	byte[] bs = new byte[256];
	for (int i = 0; i < 256; i++)
	  bs[i] = (byte) i;
	for (int i = 0; i < 4 * 5; i++)
	  fos.write(bs);
	fos.close();
	fos = null;
      }
    catch (IOException ioe)
      {
	// If we cannot even create the file just die
	harness.check(false, ioe.toString());
	harness.debug(ioe);
	return;
      }

    Process p = null;
    try
      {
	String execvm = System.getProperty("mauve.vmexec");
	if (execvm == null || execvm.equals(""))
	  harness.check(false, "mauve.vmexec system property NOT SET!");
	else
	  {
	    String cmd = execvm + " " + this.getClass().getName();
	    p = Runtime.getRuntime().exec(cmd);
	    dis = new DataInputStream(p.getInputStream());
	    dos = new DataOutputStream(p.getOutputStream());
	    br = new BufferedReader(new InputStreamReader(p.getErrorStream()));
	    // Drain error stream
	    Thread t = new Thread(this);
	    t.setDaemon(true);
	    t.start();
	    
	    dos.writeUTF(filename);
	    dos.flush();
	    
	    // Read OK message to make sure other process is ready.
	    harness.debug(dis.readUTF());
	  }
      }
    catch (IOException ioe)
      {
	// Not much will work now...
	p = null;
	harness.check(false, ioe.toString());
	harness.debug(ioe);
      }

    try
      {
	fos = new FileOutputStream(file);
	channel = fos.getChannel();
	testChannelLock("FileOutputStream", channel, false, true);
	fos.close();
	fos = null;
	
	fis = new FileInputStream(file);
	channel = fis.getChannel();
	testChannelLock("FileInputStream", channel, true, false);
	fis.close();
	fis = null;
	
	raf = new RandomAccessFile(file, "r");
	channel = raf.getChannel();
	testChannelLock("RandomAccessFile(r)", channel, true, false);
	raf.close();
	raf = null;

	raf = new RandomAccessFile(file, "rw");
	channel = raf.getChannel();
	testChannelLock("RandomAccessFile(rw)", channel, true, true);
	raf.close();
	raf = null;
      }
    catch (IOException ioe)
      {
	// Deep trouble...
	harness.debug(ioe);
	harness.check(false, ioe.toString());
      }
    finally
      {
	// Cleanup, close everything and remove test file.
	if (fos != null)
	  {
	    try
	      {
		fos.close();
	      }
	    catch (IOException ioe)
	      {
		harness.debug(ioe);
	      }
	  }
	if (fis != null)
	  {
	    try
	      {
		fis.close();
	      }
	    catch (IOException ioe)
	      {
		harness.debug(ioe);
	      }
	  }
	if (raf != null)
	  {
	    try
	      {
		raf.close();
	      }
	    catch (IOException ioe)
	      {
		harness.debug(ioe);
	      }
	  }

	try
	  {
	    if (p != null)
	      {
		dos.writeUTF("quit");
		dos.flush();
		p.destroy();
		p.waitFor();
	      }
	  }
	catch (IOException ioe)
	  {
	    harness.debug(ioe);
	  }
	catch (InterruptedException ie)
	  {
	    harness.debug(ie);
	  }

	harness.check(file.delete(), "cleanup " + file);
      }
  }

  private void testChannelLock(String what,
			       FileChannel channel,
			       boolean read, boolean write)
  {
    FileLock lock = null;
    try
      {
	harness.checkPoint(what + " lock()");
	boolean illegal;
	try
	  {
	    lock = channel.lock();
	    illegal = false;
	  }
	catch (NonWritableChannelException nrca)
	  {
	    illegal = true;
	  }
	harness.check(illegal, !write);
	harness.check(illegal || lock != null);
	if (lock != null)
	  {
	    try
	      {
		testLock(what, channel, lock, 0, Long.MAX_VALUE, false);
	      }
	    finally
	      {
		lock.release();
		harness.check(lock.isValid(), false);
		lock = null;
	      }
	  }

	harness.checkPoint(what + " tryLock()");
	try
	  {
	    lock = channel.tryLock();
	    illegal = false;
	  }
	catch (NonWritableChannelException nwce)
	  {
	    illegal = true;
	  }
	harness.check(illegal, !write);
	harness.check(illegal || lock != null);
	if (lock != null)
	  {
	    try
	      {
		testLock(what, channel, lock, 0, Long.MAX_VALUE, false);
	      }
	    finally
	      {
		lock.release();
		harness.check(lock.isValid(), false);
		lock = null;
	      }
	  }

	harness.checkPoint(what + " lock(129, 2050, false)");
	try
	  {
	    lock = channel.lock(129, 2050, false);
	    illegal = false;
	  }
	catch (NonWritableChannelException nwce)
	  {
	    illegal = true;
	  }
	harness.check(illegal, !write);
	harness.check(illegal || lock != null);
	if (lock != null)
	  {
	    try
	      {
		testLock(what, channel, lock, 129, 2050, false);
	      }
	    finally
	      {
		lock.release();
		harness.check(lock.isValid(), false);
		lock = null;
	      }
	  }
	
	harness.checkPoint(what + " tryLock(129, 2050, false)");
	try
	  {
	    lock = channel.tryLock(129, 2050, false);
	    illegal = false;
	  }
	catch (NonWritableChannelException nwce)
	  {
	    illegal = true;
	  }
	harness.check(illegal, !write);
	harness.check(illegal || lock != null);
	if (lock != null)
	  {
	    try
	      {
		testLock(what, channel, lock, 129, 2050, false);
	      }
	    finally
	      {
		lock.release();
		harness.check(lock.isValid(), false);
		lock = null;
	      }
	  }
	
	harness.checkPoint(what + " lock(129, 2050, true)");
	try
	  {
	    lock = channel.lock(129, 2050, true);
	    illegal = false;
	  }
	catch (NonReadableChannelException nrce)
	  {
	    illegal = true;
	  }
	harness.check(illegal, !read);
	harness.check(illegal || lock != null);
	if (lock != null)
	  {
	    try
	      {
		testLock(what, channel, lock, 129, 2050, true);
	      }
	    finally
	      {
		lock.release();
		harness.check(lock.isValid(), false);
		lock = null;
	      }
	  }

	harness.checkPoint(what + " tryLock(129, 2050, true)");
	try
	  {
	    lock = channel.tryLock(129, 2050, true);
	    illegal = false;
	  }
	catch (NonReadableChannelException nrce)
	  {
	    illegal = true;
	  }
	harness.check(illegal, !read);
	harness.check(illegal || lock != null);
	if (lock != null)
	  {
	    try
	      {
		testLock(what, channel, lock, 129, 2050, true);
	      }
	    finally
	      {
		lock.release();
		harness.check(lock.isValid(), false);
		lock = null;
	      }
	  }
      }
    catch(IOException ioe)
      {
	harness.debug(ioe);
	harness.check(false);
      }
  }

  private void testLock(String what,
			FileChannel channel,
			FileLock lock,
			long position,
			long size,
			boolean shared)
    throws IOException
  {
    harness.checkPoint(what + ": " + lock);
    harness.check(lock.channel(), channel);
    harness.check(lock.position(), position);
    harness.check(lock.size(), size);
    harness.check(lock.isShared(), shared);
    harness.check(lock.isValid(), true);
    harness.check(lock.overlaps(0, Long.MAX_VALUE), true);
    harness.check(lock.overlaps(position, size), true);
    if (position < Long.MAX_VALUE)
      harness.check(lock.overlaps(0, position + 1), true);
    if (position > 0)
      {
	harness.check(lock.overlaps(0, 1), false);
	harness.check(lock.overlaps(0, position - 1), false);
      }
    if (size < Long.MAX_VALUE && position > 0)
      harness.check(lock.overlaps(position - 1, size + 1), true);
    if (size > 1)
      harness.check(lock.overlaps(position, size - 1), true);

    // Let the other process try to lock some things.
    if (dos != null)
      {
	dos.writeUTF("lock");
	harness.debug("Sending: "
		      + position + ", " + size + " (" + shared + ")");
	harness.debug("lock: " + lock);
	dos.writeLong(position);
	dos.writeLong(size);
	dos.writeBoolean(shared);
	dos.flush();

	harness.debug("EXTERNAL: " + dis.readUTF());
	harness.check(dis.readBoolean(), what + " external " + lock);
      }
  }

  public void run()
  {
    // Drain error stream from external process.
    // Null input (EOFException) will stop loop.
    try
      {
	String line = br.readLine();
	while (line != null)
	  {
	    harness.debug(" ex: " + line);
	    line = br.readLine();
	  }
      }
    catch(IOException ioe)
      {
      }
  }

  public static void main(String[] args) throws Exception
  {
    DataInputStream dis = new DataInputStream(System.in);
    DataOutputStream dos = new DataOutputStream(System.out);

    String file = dis.readUTF();
    RandomAccessFile raf = new RandomAccessFile(file, "rw");
    FileChannel channel = raf.getChannel();

    // Let the other process know we are ready.
    dos.writeUTF("Opened file: " + file);
    dos.flush();

    try
      {
	String command = dis.readUTF();
	while (command != null && !command.equals("quit"))
	  {
	    long position = dis.readLong();
	    long size = dis.readLong();
	    boolean shared = dis.readBoolean();
	    
	    // This will be printed by the main process with harness.debug()
	    System.err.println("Recv: "
			       + position + ", " + size + " (" + shared + ")");

	    String what = "lock[" + position + "," + size + "," + shared + "]";
	    String message = "X";
	    boolean result = true;
	    
	    // Try to get the same (and exclusive) lock
	    // This should always fail.
	    FileLock lock = null;
	    try
	      {
		lock = channel.tryLock(position, size, false);
		if (lock != null)
		  {
		    result = false;
		    message = "Got lock: " + lock;
		  }
	      }
	    finally
	      {
		// Clean up anyway
		try
		  {
		    lock.release();
		    lock = null;
		  }
		catch(Throwable t)
		  {
		  }
	      }

	    // If we couldn't get an exclusive lock and the lock is shared,
	    // we should be able to get a shared lock
	    if (result && shared)
	      {
		try
		  {
		    lock = channel.tryLock(position, size, true);
		    if (lock == null)
		      {
			result = false;
			message = "Couldn't get shared lock";
		      }
		  }
		finally
		  {
		    // Clean up
		    try
		      {
			lock.release();
			lock = null;
		      }
		    catch(Throwable t)
		      {
		      }
		  }
	      }

	    if (result)
	      dos.writeUTF("OK: " + what);
	    else
	      dos.writeUTF("Failed: " + what + ": " + message);
	    dos.writeBoolean(result);
	    dos.flush();

	    command = dis.readUTF();
	  }
      }
    catch (Throwable t)
      {
	// Urgh
	dos.writeUTF("External error: " + t.toString());
	t.printStackTrace();
	System.err.flush();
      }
  }
}
