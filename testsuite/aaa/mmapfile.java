/* { dg-output "OK" } */

package aaa;

import java.io.*;
import java.nio.*;
import java.nio.channels.FileChannel;
import java.nio.file.*;

// FileChannel.map: a MappedByteBuffer over a file (the Kotlin compiler
// reads jars this way).
public class mmapfile
{
    public static void main(String[] args) throws Exception
    {
        Path p = Files.createTempFile("mmapfile", ".bin");
        Files.write(p, "hello, mapped world".getBytes("UTF-8"));
        String read;
        try (FileChannel ch = FileChannel.open(p, StandardOpenOption.READ)) {
            MappedByteBuffer buf = ch.map(FileChannel.MapMode.READ_ONLY, 7, 6);
            byte[] b = new byte[buf.remaining()];
            buf.get(b);
            read = new String(b, "UTF-8") + " " + ch.map(FileChannel.MapMode.READ_ONLY, 0, 5).get(4);
        }
        Files.delete(p);
        System.out.println("mapped 111".equals(read) ? "OK" : "FAIL: " + read);
    }
}
