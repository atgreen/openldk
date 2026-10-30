// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

import java.io.*;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.security.Permission;
import java.util.*;

public class FileUrlProbe {
    static int checks;
    static void check(boolean value, String message) {
        checks++;
        if (!value) throw new AssertionError(message);
    }
    public static void main(String[] args) throws Exception {
        Path directory = Files.createTempDirectory("openldk-file-url-");
        Path path = directory.resolve("snow 雪 + percent %.txt");
        byte[] contents = "file contents\n".getBytes(StandardCharsets.UTF_8);
        try {
            Files.write(path, contents);
            URL url = path.toUri().toURL();
            URLConnection connection = url.openConnection();
            check(!(connection instanceof JarURLConnection), "file connection type");
            check(connection.getURL().equals(url), "connection URL");
            Permission permission = connection.getPermission();
            check(permission.equals(new FilePermission(path.toString(), "read")), "decoded file permission");
            check(connection.getContentLengthLong() == contents.length, "content length");
            check(connection.getContentLength() == contents.length, "integer content length");
            check(connection.getLastModified() > 0, "last modified");
            check(connection.getContentType().equals("text/plain"), "content type");
            check(connection.getHeaderField("content-length").equals("" + contents.length), "length header");
            check(connection.getHeaderFields().get("content-length").get(0).equals("" + contents.length), "headers map");
            try (InputStream stream = connection.getInputStream()) {
                check(Arrays.equals(stream.readAllBytes(), contents), "file connection reads decoded path");
            }
            try (InputStream stream = url.openStream()) {
                check(Arrays.equals(stream.readAllBytes(), contents), "URL.openStream uses decoded file path");
            }
            URL referenced = new URL(url.toExternalForm() + "?ignored=value#fragment");
            try (InputStream stream = referenced.openStream()) {
                check(Arrays.equals(stream.readAllBytes(), contents), "file query and fragment do not alter path");
            }
            URL resource = ClassLoader.getSystemResource("FileUrlProbe.class");
            check(resource != null && resource.getProtocol().equals("file"), "resource URL");
            URLConnection resourceConnection = resource.openConnection();
            check(!(resourceConnection instanceof JarURLConnection), "resource connection type");
            check(resourceConnection.getPermission() instanceof FilePermission, "resource permission");
            try (InputStream stream = resource.openStream()) {
                check(stream.read() == 0xca && stream.read() == 0xfe, "resource stream class magic");
            }
            URLConnection relative = new URL("file:missing-relative-file").openConnection();
            check(relative.getPermission().equals(new FilePermission("missing-relative-file", "read")),
                  "relative permission does not open missing file");
            URL missing = directory.resolve("missing").toUri().toURL();
            check(missing.openConnection().getPermission().equals(
                  new FilePermission(directory.resolve("missing").toString(), "read")), "missing absolute permission");
            URLConnection listing = directory.toUri().toURL().openConnection();
            check(!(listing instanceof JarURLConnection), "directory connection type");
            check(listing.getPermission().equals(new FilePermission(directory.toString() + File.separator, "read")),
                  "directory permission");
            try (InputStream stream = listing.getInputStream()) {
                check(new String(stream.readAllBytes(), StandardCharsets.UTF_8).contains(path.getFileName().toString()),
                      "directory listing");
            }
        } finally {
            Files.deleteIfExists(path);
            Files.deleteIfExists(directory);
        }
        System.out.println("File-URL permissions, metadata, headers and streams: " + checks + " checks passed");
    }
}
