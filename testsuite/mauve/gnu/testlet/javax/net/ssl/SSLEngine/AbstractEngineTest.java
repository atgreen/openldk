/* AbstractEngineTest.java -- base SSLEngine test.
   Copyright (C) 2006  Casey Marshall <csm@gnu.org>

This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: not-a-test

package gnu.testlet.javax.net.ssl.SSLEngine;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;

import javax.net.ssl.KeyManager;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLException;
import javax.net.ssl.TrustManager;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.net.ssl.SSLEngineResult.Status;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public abstract class AbstractEngineTest
  implements Testlet
{
  protected static final String TEST_MESSAGE = "Hello, world!";
  protected SSLContext context;
  protected SSLEngine clientEngine;
  protected SSLEngine serverEngine;

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public final void test(TestHarness harness)
  {
    if (setup(harness))
      implTest(harness);
  }
  
  protected boolean setup(TestHarness harness)
  {
    try
      {
        harness.checkPoint("SSLContext.getInstance");
        context = SSLContext.getInstance("SSL");
        context.init(new KeyManager[] { new SimpleX509KeyManager() },
                     new TrustManager[] { new SimpleX509TrustManager() },
                     SecureRandom.getInstance("Fortuna"));
      }
    catch (Exception e)
      {
        harness.fail("SSLContext.getInstance");
        harness.debug(e);
        return false;
      }
    return true;
  }
  
  protected boolean setupEngines(TestHarness harness)
  {
    serverEngine = context.createSSLEngine();
    clientEngine = context.createSSLEngine();
    harness.check(serverEngine != null);
    harness.check(clientEngine != null);
    
    serverEngine.setUseClientMode(false);
    clientEngine.setUseClientMode(true);
    return true;
  }

  protected abstract void implTest(TestHarness harness);
  
  protected void runHandshake() throws SSLException
  {
    ByteBuffer empty = ByteBuffer.allocate(0);
    ByteBuffer cnetBuffer = ByteBuffer.allocate(clientEngine.getSession().getPacketBufferSize());
    ByteBuffer snetBuffer = ByteBuffer.allocate(serverEngine.getSession().getPacketBufferSize());
    
    clientEngine.beginHandshake();
    serverEngine.beginHandshake();
    
    SSLEngineResult result = null;
    SSLEngineResult.HandshakeStatus srv = serverEngine.getHandshakeStatus();
    SSLEngineResult.HandshakeStatus cli = clientEngine.getHandshakeStatus();
    
    while (srv != HandshakeStatus.NOT_HANDSHAKING
           && cli != HandshakeStatus.NOT_HANDSHAKING)
      {
        if (cli == HandshakeStatus.NEED_WRAP)
          {
            if (srv != HandshakeStatus.NEED_UNWRAP)
              {
                throw new SSLException("invalid server handshake state: " + srv);
              }
            
            result = clientEngine.wrap(empty, cnetBuffer);
            if (result.getStatus() != Status.OK)
              throw new SSLException("unexpected status after wrap: "
                                     + result.getStatus());
            cli = result.getHandshakeStatus();
            cnetBuffer.flip();
            result = serverEngine.unwrap(cnetBuffer, empty);
            cnetBuffer.compact();
            if (result.getStatus() != Status.OK)
              throw new SSLException("unexpected status after unwrap: "
                                     + result.getStatus());
            srv = result.getHandshakeStatus();
            
            if (cli == HandshakeStatus.NEED_TASK)
              {
                Runnable task = null;
                while ((task = clientEngine.getDelegatedTask()) != null)
                  task.run();
                cli = clientEngine.getHandshakeStatus();
              }
            
            if (srv == HandshakeStatus.NEED_TASK)
              {
                Runnable task = null;
                while ((task = serverEngine.getDelegatedTask()) != null)
                  task.run();
                srv = serverEngine.getHandshakeStatus();
              }
          }
        else if (cli == HandshakeStatus.NEED_UNWRAP)
          {
            if (srv != HandshakeStatus.NEED_WRAP)
              {
                throw new SSLException("invalid server handshake state: " + srv);
              }
            
            result = serverEngine.wrap(empty, snetBuffer);
            if (result.getStatus() != Status.OK)
              throw new SSLException("unexpected status after wrap: "
                                     + result.getStatus());
            srv = result.getHandshakeStatus();
            snetBuffer.flip();
            result = clientEngine.unwrap(snetBuffer, empty);
            snetBuffer.compact();
            if (result.getStatus() != Status.OK)
              throw new SSLException("unexpected status after unwrap: "
                                     + result.getStatus());
            cli = result.getHandshakeStatus();

            if (cli == HandshakeStatus.NEED_TASK)
              {
                Runnable task = null;
                while ((task = clientEngine.getDelegatedTask()) != null)
                  task.run();
                cli = clientEngine.getHandshakeStatus();
              }
            
            if (srv == HandshakeStatus.NEED_TASK)
              {
                Runnable task = null;
                while ((task = serverEngine.getDelegatedTask()) != null)
                  task.run();
                srv = serverEngine.getHandshakeStatus();
              }
          }
        else if (cli == HandshakeStatus.NEED_TASK)
          {
            throw new SSLException("invalid initial state: " + cli);
          }
        else if (cli == HandshakeStatus.FINISHED)
          {
            if (srv != HandshakeStatus.FINISHED)
              throw new SSLException("invalid final server state: " + srv);
            break;
          }
      }
    
    ByteBuffer appBuffer = ByteBuffer.allocate(serverEngine.getSession().getApplicationBufferSize());
    Charset cs = Charset.forName("US-ASCII");
    CharsetEncoder enc = cs.newEncoder();
    enc.encode(CharBuffer.wrap(TEST_MESSAGE), appBuffer, true);
    appBuffer.flip();
    result = clientEngine.wrap(appBuffer, cnetBuffer);
    if (result.getStatus() != Status.OK)
      throw new SSLException("unexpected status: " + result.getStatus());
    cnetBuffer.flip();
    appBuffer.clear();
    result = serverEngine.unwrap(cnetBuffer, appBuffer);
    if (result.getStatus() != Status.OK)
      throw new SSLException("unexpected status: " + result.getStatus());
    appBuffer.flip();
    String msg = cs.decode(appBuffer).toString();
    if (!msg.equals(TEST_MESSAGE))
      throw new SSLException("message decode failed");

    appBuffer.clear();
    enc.encode(CharBuffer.wrap(msg), appBuffer, true);
    appBuffer.flip();
    result = serverEngine.wrap(appBuffer, snetBuffer);
    if (result.getStatus() != Status.OK)
      throw new SSLException("unexpected status: " + result.getStatus());
    snetBuffer.flip();
    appBuffer.clear();
    result = clientEngine.unwrap(snetBuffer, appBuffer);
    if (result.getStatus() != Status.OK)
      throw new SSLException("unexpected status: " + result.getStatus());
    appBuffer.flip();
    msg = cs.decode(appBuffer).toString();
    if (!msg.equals(TEST_MESSAGE))
      throw new SSLException("message decode (2) failed");
  }
}
