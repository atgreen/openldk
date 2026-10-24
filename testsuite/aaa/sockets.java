/* { dg-output "OK" } */

package aaa;

import java.io.*;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.channels.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

// What Tomcat's NIO connector and its HTTP clients need: blocking sockets
// (java.net, built on sun.nio.ch.Net), a selector over non-blocking
// channels, socket options, and the errors a JVM reports.
public class sockets
{
    static String echoOverSockets() throws Exception
    {
        try (ServerSocket server = new ServerSocket(0, 50, InetAddress.getLoopbackAddress())) {
            int port = server.getLocalPort();
            Thread t = new Thread(() -> {
                try (Socket s = server.accept();
                     BufferedReader in = new BufferedReader(new InputStreamReader(s.getInputStream()));
                     PrintWriter out = new PrintWriter(s.getOutputStream(), true)) {
                    out.println("echo:" + in.readLine() + ":" + (s.getPort() > 0));
                } catch (IOException e) {
                    e.printStackTrace();
                }
            });
            t.start();
            try (Socket c = new Socket("localhost", port)) {
                c.setTcpNoDelay(true);
                c.setSoTimeout(10000);
                PrintWriter out = new PrintWriter(c.getOutputStream(), true);
                out.println("hello");
                String line = new BufferedReader(new InputStreamReader(c.getInputStream())).readLine();
                t.join();
                return line + ":" + c.getTcpNoDelay() + ":" + (c.getLocalPort() > 0)
                    + ":" + c.getInetAddress().getHostAddress();
            }
        }
    }

    // One selector thread serving two clients, as Tomcat's poller does.
    static String selectorServer() throws Exception
    {
        Selector selector = Selector.open();
        ServerSocketChannel server = ServerSocketChannel.open();
        server.bind(new InetSocketAddress("127.0.0.1", 0));
        server.configureBlocking(false);
        server.register(selector, SelectionKey.OP_ACCEPT);
        int port = ((InetSocketAddress) server.getLocalAddress()).getPort();

        List<String> replies = Collections.synchronizedList(new ArrayList<>());
        Thread[] clients = new Thread[2];
        for (int i = 0; i < clients.length; i++) {
            String name = "client" + i;
            clients[i] = new Thread(() -> {
                try (SocketChannel c = SocketChannel.open(new InetSocketAddress("127.0.0.1", port))) {
                    c.write(ByteBuffer.wrap((name + "\n").getBytes(StandardCharsets.US_ASCII)));
                    ByteBuffer buf = ByteBuffer.allocate(64);
                    while (c.read(buf) >= 0 && (buf.position() == 0 || buf.get(buf.position() - 1) != '\n'))
                        ;
                    replies.add(new String(buf.array(), 0, buf.position(), StandardCharsets.US_ASCII).trim());
                } catch (IOException e) {
                    e.printStackTrace();
                }
            });
            clients[i].start();
        }

        int served = 0;
        while (served < clients.length) {
            selector.select(10000);
            Iterator<SelectionKey> it = selector.selectedKeys().iterator();
            while (it.hasNext()) {
                SelectionKey key = it.next();
                it.remove();
                if (key.isAcceptable()) {
                    SocketChannel ch = server.accept();
                    if (ch != null) {
                        ch.configureBlocking(false);
                        ch.register(selector, SelectionKey.OP_READ, ByteBuffer.allocateDirect(64));
                    }
                } else if (key.isReadable()) {
                    SocketChannel ch = (SocketChannel) key.channel();
                    ByteBuffer buf = (ByteBuffer) key.attachment();
                    if (ch.read(buf) < 0 || buf.get(buf.position() - 1) == '\n') {
                        buf.flip();
                        byte[] got = new byte[buf.remaining()];
                        buf.get(got);
                        ByteBuffer[] reply = { ByteBuffer.wrap("got ".getBytes(StandardCharsets.US_ASCII)),
                                               ByteBuffer.wrap(got) };
                        while (reply[1].hasRemaining())
                            ch.write(reply);
                        key.cancel();
                        ch.close();
                        served++;
                    }
                }
            }
        }
        for (Thread t : clients)
            t.join();
        selector.close();
        server.close();
        Collections.sort(replies);
        return replies.toString();
    }

    static String wakeup() throws Exception
    {
        try (Selector selector = Selector.open()) {
            new Thread(() -> {
                try { Thread.sleep(200); } catch (InterruptedException e) { }
                selector.wakeup();
            }).start();
            long t0 = System.nanoTime();
            int n = selector.select(20000);
            return "woke:" + n + ":" + ((System.nanoTime() - t0) < 10_000_000_000L);
        }
    }

    static String errors() throws Exception
    {
        List<String> got = new ArrayList<>();
        int closedPort;
        try (ServerSocket s = new ServerSocket(0, 50, InetAddress.getLoopbackAddress())) {
            closedPort = s.getLocalPort();
            try (ServerSocket again = new ServerSocket()) {
                again.setReuseAddress(false);
                again.bind(new InetSocketAddress(InetAddress.getLoopbackAddress(), closedPort));
                got.add("bound twice");
            } catch (BindException e) {
                got.add("BindException");
            }
        }
        try (Socket c = new Socket(InetAddress.getLoopbackAddress(), closedPort)) {
            got.add("connected");
        } catch (ConnectException e) {
            got.add("ConnectException");
        }
        try (ServerSocket s = new ServerSocket(0, 50, InetAddress.getLoopbackAddress())) {
            s.setSoTimeout(100);
            s.accept();
            got.add("accepted");
        } catch (SocketTimeoutException e) {
            got.add("SocketTimeoutException");
        }
        return got.toString();
    }

    public static void main(String[] args) throws Exception
    {
        List<String> got = new ArrayList<>();
        got.add(InetAddress.getByName("localhost").getHostAddress());
        got.add(Arrays.toString(InetAddress.getByName("127.0.0.1").getAddress()));
        got.add(echoOverSockets());
        got.add(selectorServer());
        got.add(wakeup());
        got.add(errors());
        String expected = "[127.0.0.1, [127, 0, 0, 1], echo:hello:true:true:true:127.0.0.1, "
            + "[got client0, got client1], woke:0:true, "
            + "[BindException, ConnectException, SocketTimeoutException]]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
