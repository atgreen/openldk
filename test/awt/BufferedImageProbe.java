// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.image.BufferedImage;

public class BufferedImageProbe {
    public static void main(String[] args) {
        for (int type : new int[] {BufferedImage.TYPE_INT_ARGB,
                BufferedImage.TYPE_INT_RGB, BufferedImage.TYPE_INT_BGR}) {
            BufferedImage image = new BufferedImage(12, 10, type);
            int expected = type == BufferedImage.TYPE_INT_ARGB ? 0x80402010 : 0xff402010;
            image.setRGB(3, 4, 0x80402010);
            if (image.getRGB(3, 4) != expected) throw new AssertionError("pixel type " + type);
            BufferedImage child = image.getSubimage(2, 3, 4, 5);
            if (child.getRGB(1, 1) != expected) throw new AssertionError("child offset");
            child.setRGB(0, 0, 0xff123456);
            if (image.getRGB(2, 3) != 0xff123456) throw new AssertionError("shared raster");
            int[] pixels = {0xff000001, 0xff000002, 0xff000003, 0xff000004};
            image.setRGB(5, 6, 2, 2, pixels, 0, 2);
            int[] actual = image.getRGB(5, 6, 2, 2, null, 0, 2);
            if (!java.util.Arrays.equals(pixels, actual)) throw new AssertionError("bulk pixels");
            if (image.getWidth() != 12 || image.getHeight() != 10) throw new AssertionError("dimensions");
        }
        System.out.println("BufferedImage pixels, shared subimages, and bulk access passed");
    }
}
