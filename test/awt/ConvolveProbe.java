// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.image.*;
import java.io.*;
import java.util.Arrays;

public class ConvolveProbe {
    static void write(DataOutputStream out, Raster image) throws IOException {
        int[] samples = image.getPixels(image.getMinX(), image.getMinY(), image.getWidth(), image.getHeight(), (int[]) null);
        for (int sample : samples) out.writeInt(sample);
    }
    public static void main(String[] args) throws Exception {
        Kernel[] kernels = {
            new Kernel(3, 1, new float[]{.125f, .25f, .625f}),
            new Kernel(1, 3, new float[]{.125f, .25f, .625f}),
            new Kernel(2, 2, new float[]{.125f, .25f, .125f, .5f}),
            new Kernel(3, 1, new float[]{-.5f, 2, -.5f}),
            new Kernel(5, 1, new float[]{.0625f, .25f, .375f, .25f, .0625f})
        };
        try (DataOutputStream out = new DataOutputStream(new FileOutputStream(args[0]))) {
            for (int type : new int[]{1, 2, 3, 6, 10, 11}) {
                BufferedImage input = new BufferedImage(9, 7, type);
                for (int y = 0; y < 7; y++) for (int x = 0; x < 9; x++) {
                    int a = 60 + (x * 29 + y * 17) % 196;
                    input.setRGB(x, y, a << 24 | ((x * 31 + y * 13) % 256) << 16
                            | ((x * 11 + y * 37) % 256) << 8 | (x * 47 + y * 19) % 256);
                    if (type == BufferedImage.TYPE_USHORT_GRAY)
                        input.getRaster().setSample(x, y, 0, (x * 4001 + y * 997) % 65536);
                }
                int[] before = input.getRGB(0, 0, 9, 7, null, 0, 9);
                for (Kernel kernel : kernels) for (int edge : new int[]{0, 1}) {
                    ConvolveOp operation = new ConvolveOp(kernel, edge, null);
                    BufferedImage result = operation.filter(input, null);
                    write(out, result.getRaster());
                    if (type == BufferedImage.TYPE_BYTE_GRAY || type == BufferedImage.TYPE_USHORT_GRAY)
                        write(out, operation.filter(input.getRaster(), null));
                    if (!Arrays.equals(before, input.getRGB(0, 0, 9, 7, null, 0, 9)))
                        throw new AssertionError("Convolution changed source");
                }
            }
        }
        System.out.println("Convolution kernels, edges, alpha, grayscale, and rasters passed");
    }
}
