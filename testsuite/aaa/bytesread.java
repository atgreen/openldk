/* { dg-output "OK" } */

package aaa;

import java.nio.charset.StandardCharsets;
import java.nio.file.*;

// Bytes of 0x80 and above read back from a file as the signed bytes they
// are (Files.readAllBytes, readString: Scala reads source files this way).
// OpenLDK stored them unsigned, so UTF-8 text came back garbled.
public class bytesread
{
    public static void main(String[] args) throws Exception
    {
        byte[] b = "héllo wörld €".getBytes(StandardCharsets.UTF_8);
        Path p = Files.createTempFile("bytesread", ".txt");
        Files.write(p, b);
        boolean same = java.util.Arrays.equals(b, Files.readAllBytes(p));
        String text = Files.readString(p);
        Files.delete(p);
        String want = "héllo wörld €";
        System.out.println(same && want.equals(text) ? "OK" : "FAIL: " + same + " " + text.length());
    }
}
