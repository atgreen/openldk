// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.*;
import java.net.*;
import java.nio.*;
import java.nio.channels.*;
import java.util.Arrays;

public class DatagramProbe {
    static final InetAddress LOOP;
    static {
        try { LOOP = InetAddress.getByName("127.0.0.1"); }
        catch (Exception e) { throw new ExceptionInInitializerError(e); }
    }
    static void check(boolean value, String message) {
        if (!value) throw new AssertionError(message);
    }
    static void sockets() throws Exception {
        try (DatagramSocket sender = new DatagramSocket(new InetSocketAddress(LOOP, 0));
             DatagramSocket receiver = new DatagramSocket(new InetSocketAddress(LOOP, 0))) {
            receiver.setSoTimeout(10000);
            byte[] input = { 9, 1, 2, 3, 8 }, output = new byte[10];
            Arrays.fill(output, (byte)90);
            sender.send(new DatagramPacket(input, 1, 3, receiver.getLocalSocketAddress()));
            DatagramPacket packet = new DatagramPacket(output, 2, 5);
            receiver.receive(packet);
            check(packet.getLength() == 3 && packet.getOffset() == 2, "packet bounds");
            check(packet.getPort() == sender.getLocalPort() && packet.getAddress().equals(LOOP), "sender address");
            check(Arrays.equals(output, new byte[] {90,90,1,2,3,90,90,90,90,90}), "offset transfer");
            sender.send(new DatagramPacket(new byte[] {4,5,6,7}, 4, receiver.getLocalSocketAddress()));
            packet = new DatagramPacket(output, 1, 2); receiver.receive(packet);
            check(packet.getLength() == 2 && output[1] == 4 && output[2] == 5, "packet truncation");
            sender.send(new DatagramPacket(new byte[] {11}, 1, receiver.getLocalSocketAddress()));
            packet = new DatagramPacket(output, output.length); receiver.receive(packet);
            check(packet.getLength() == 1 && output[0] == 11, "datagram boundary");
            sender.send(new DatagramPacket(new byte[0], 0, receiver.getLocalSocketAddress()));
            receiver.receive(packet);
            check(packet.getLength() == 0 && packet.getPort() == sender.getLocalPort(), "empty datagram");
            receiver.setSoTimeout(80);
            try { receiver.receive(packet); throw new AssertionError("missing receive timeout"); }
            catch (SocketTimeoutException expected) { }
        }
    }
    static long read(DatagramChannel channel, ByteBuffer[] buffers) throws Exception {
        long deadline = System.nanoTime() + 10_000_000_000L;
        for (;;) {
            long n = channel.read(buffers);
            if (n != 0) return n;
            if (System.nanoTime() > deadline) throw new AssertionError("missing connected datagram");
            Thread.sleep(1);
        }
    }
    static void channels() throws Exception {
        try (DatagramChannel sender = DatagramChannel.open(StandardProtocolFamily.INET);
             DatagramChannel receiver = DatagramChannel.open(StandardProtocolFamily.INET)) {
            sender.bind(new InetSocketAddress(LOOP, 0)); receiver.bind(new InetSocketAddress(LOOP, 0));
            SocketAddress original = sender.getLocalAddress();
            receiver.configureBlocking(false);
            check(receiver.receive(ByteBuffer.allocate(8)) == null, "empty nonblocking receive");
            sender.connect(receiver.getLocalAddress()); receiver.connect(sender.getLocalAddress());
            check(sender.write(ByteBuffer.wrap(new byte[] {1,2,3})) == 3, "connected send");
            ByteBuffer head = ByteBuffer.allocateDirect(1), tail = ByteBuffer.allocate(8);
            check(read(receiver, new ByteBuffer[] {head,tail}) == 3, "scattering receive count");
            check(head.get(0) == 1 && tail.get(0) == 2 && tail.get(1) == 3, "scattering data");
            check(sender.write(new ByteBuffer[] {ByteBuffer.wrap(new byte[] {4}),
                                                  ByteBuffer.wrap(new byte[] {5,6})}) == 3, "gathering send");
            head.clear(); tail.clear();
            check(read(receiver, new ByteBuffer[] {head,tail}) == 3 && head.get(0) == 4
                  && tail.get(0) == 5 && tail.get(1) == 6, "gathering packet boundary");
            sender.disconnect();
            check(!sender.isConnected() && sender.getLocalAddress().equals(original), "disconnect preserves bind");
            check(sender.send(ByteBuffer.wrap(new byte[] {7}), receiver.getLocalAddress()) == 1, "send after disconnect");
            head.clear(); check(read(receiver, new ByteBuffer[] {head}) == 1 && head.get(0) == 7, "receive after disconnect");
            sender.connect(receiver.getLocalAddress());
            try { sender.write(ByteBuffer.allocate(65508)); throw new AssertionError("oversized datagram accepted"); }
            catch (IOException expected) { }
            check(sender.write(ByteBuffer.wrap(new byte[] {12})) == 1, "send after rejected packet");
            head.clear(); check(read(receiver, new ByteBuffer[] {head}) == 1 && head.get(0) == 12, "rejected packet changed queue");
        }
    }
    @SuppressWarnings("deprecation")
    static void membership() throws Exception {
        try (MulticastSocket socket = new MulticastSocket(0)) {
            InetAddress group = InetAddress.getByName("239.255.0.1");
            socket.setTimeToLive(3); check(socket.getTimeToLive() == 3, "multicast TTL");
            socket.joinGroup(group); socket.leaveGroup(group);
            NetworkInterface selected = NetworkInterface.getByInetAddress(LOOP);
            socket.setNetworkInterface(selected);
            check(socket.getNetworkInterface().getIndex() == selected.getIndex(), "multicast interface");
        }
        try (DatagramChannel channel = DatagramChannel.open(StandardProtocolFamily.INET)) {
            channel.bind(new InetSocketAddress(0));
            InetAddress group = InetAddress.getByName("239.255.0.2");
            NetworkInterface selected = NetworkInterface.getByInetAddress(LOOP);
            MembershipKey key = channel.join(group, selected);
            check(key.isValid() && key.sourceAddress() == null, "all-source membership");
            key.block(LOOP); key.unblock(LOOP); key.drop();
            check(!key.isValid(), "dropped membership still valid");
            MembershipKey specific = channel.join(group, selected, LOOP);
            check(specific.isValid() && specific.sourceAddress().equals(LOOP), "source-specific membership");
            specific.drop();
            check(!specific.isValid(), "source-specific drop");
        }
    }
    static void accept() throws Exception {
        try (ServerSocket server = new ServerSocket(0, 1, LOOP)) {
            server.setSoTimeout(10000);
            // Complete the kernel connection before accept, avoiding a race
            // between a short legacy timeout and cold client-side compilation.
            try (Socket client = new Socket(LOOP, server.getLocalPort()); Socket peer = server.accept()) {
                check(peer.getSoTimeout() == 0, "accepted socket inherited timeout");
                client.getOutputStream().write(new byte[] {8,9,10});
                check(Arrays.equals(peer.getInputStream().readNBytes(3), new byte[] {8,9,10}), "TCP data");
            }
        }
    }
    public static void main(String[] args) throws Exception {
        sockets(); channels(); membership(); accept();
        System.out.println("UDP packets, offsets, empty messages, vectors, multicast, disconnect, and TCP accept passed");
    }
}
