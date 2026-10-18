/* { dg-output "55381b8f(\n|\r\n|\r)hello hello hello zlib" } */

package aaa;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.zip.CRC32;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

// java.util.zip's natives call the system zlib.
public class zlib
{
    public static void main(String[] args) throws Exception
    {
        byte[] data = "hello hello hello zlib".getBytes("UTF-8");
        CRC32 crc = new CRC32();
        crc.update(data);
        System.out.println(Long.toHexString(crc.getValue()));
        ByteArrayOutputStream bo = new ByteArrayOutputStream();
        try (GZIPOutputStream g = new GZIPOutputStream(bo)) { g.write(data); }
        try (GZIPInputStream g = new GZIPInputStream(new ByteArrayInputStream(bo.toByteArray()))) {
            System.out.println(new String(g.readAllBytes(), "UTF-8"));
        }
    }
}
