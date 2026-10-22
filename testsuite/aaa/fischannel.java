/* { dg-output "OK" } */

package aaa;

import java.io.*;
import java.nio.*;
import java.nio.channels.*;
import java.nio.charset.*;

// Reading a FileInputStream's channel into a heap ByteBuffer and decoding
// it, as scalac reads source files.  OpenLDK keeps a Lisp stream in the
// stream's FileDescriptor, which the channel then passed to read(2).
public class fischannel
{
    public static void main(String[] args) throws Exception
    {
        File f = File.createTempFile("fischannel", ".txt");
        try (Writer w = new OutputStreamWriter(new FileOutputStream(f), StandardCharsets.UTF_8)) {
            w.write("object Main { println(\"hé\") }\n");
        }
        StringBuilder text = new StringBuilder();
        try (FileInputStream in = new FileInputStream(f)) {
            ReadableByteChannel ch = in.getChannel();
            CharsetDecoder dec = StandardCharsets.UTF_8.newDecoder();
            ByteBuffer bytes = ByteBuffer.allocate(16);
            CharBuffer chars = CharBuffer.allocate(64);
            boolean eof = false;
            while (!eof) {
                eof = ch.read(bytes) < 0;
                bytes.flip();
                dec.decode(bytes, chars, eof);
                bytes.compact();
            }
            dec.flush(chars);
            chars.flip();
            text.append(chars);
        }
        f.delete();
        String want = "object Main { println(\"hé\") }\n";
        System.out.println(want.equals(text.toString()) ? "OK" : "FAIL: [" + text + "]");
    }
}
