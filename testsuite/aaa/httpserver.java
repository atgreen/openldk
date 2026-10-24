/* { dg-output "OK" } */

package aaa;

import com.sun.net.httpserver.*;
import java.io.*;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

// A whole HTTP exchange: the JDK's HttpServer (a selector over
// non-blocking channels, as Tomcat's NIO connector) answering
// HttpURLConnection, GET and POST, over one kept-alive connection.
public class httpserver
{
    public static void main(String[] args) throws Exception
    {
        HttpServer server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
        server.createContext("/hello", exchange -> {
            byte[] body;
            if (exchange.getRequestMethod().equals("POST"))
                body = ("posted " + new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8))
                    .getBytes(StandardCharsets.UTF_8);
            else
                body = ("hello " + exchange.getRequestURI().getQuery()).getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().add("Content-Type", "text/plain");
            exchange.sendResponseHeaders(200, body.length);
            try (OutputStream out = exchange.getResponseBody()) {
                out.write(body);
            }
        });
        server.start();
        int port = server.getAddress().getPort();

        List<String> got = new ArrayList<>();
        for (int i = 0; i < 2; i++) {
            HttpURLConnection c = (HttpURLConnection) new URL("http://localhost:" + port + "/hello?n=" + i).openConnection();
            got.add(c.getResponseCode() + " " + c.getContentType() + " "
                    + new String(c.getInputStream().readAllBytes(), StandardCharsets.UTF_8));
        }
        HttpURLConnection post = (HttpURLConnection) new URL("http://localhost:" + port + "/hello").openConnection();
        post.setRequestMethod("POST");
        post.setDoOutput(true);
        try (OutputStream out = post.getOutputStream()) {
            out.write("data".getBytes(StandardCharsets.UTF_8));
        }
        got.add(post.getResponseCode() + " " + new String(post.getInputStream().readAllBytes(), StandardCharsets.UTF_8));
        HttpURLConnection missing = (HttpURLConnection) new URL("http://localhost:" + port + "/nothing").openConnection();
        got.add(String.valueOf(missing.getResponseCode()));
        server.stop(0);

        String expected = "[200 text/plain hello n=0, 200 text/plain hello n=1, 200 posted data, 404]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
