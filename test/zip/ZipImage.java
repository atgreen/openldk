/* Copyright (C) 2026 Anthony Green
 * SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
import java.io.*;
import java.util.zip.*;
public class ZipImage {
    static ZipFile archive;
    static InputStream stored, deflated;
    static FileInputStream file;
    public static void main(String[] args) throws Exception {
        if (archive == null) {
            archive = new ZipFile(args[0]);
            stored = archive.getInputStream(archive.getEntry("stored.bin"));
            deflated = archive.getInputStream(archive.getEntry("deflated.bin"));
            file = new FileInputStream(args[0]);
            if (stored.read() != 0 || deflated.read() != 0 || file.read() != 80)
                throw new AssertionError("Warmup reads");
        } else {
            if (stored.read() != 1 || deflated.read() != 1 || file.read() != 75)
                throw new AssertionError("Restored positions");
            if (stored.readAllBytes().length != 254 || deflated.readAllBytes().length != 2558)
                throw new AssertionError("Restored entry contents");
            archive.close(); file.close();
            System.out.println("ZIP image passed");
        }
    }
}
