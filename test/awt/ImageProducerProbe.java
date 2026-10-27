// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.awt.*;
import java.awt.image.*;

public class ImageProducerProbe {
    public static void main(String[] args) throws Exception {
        int[] pixels = {0xff000000, 0xff400000, 0xff800000};
        ImageProducer source = new MemoryImageSource(3, 1, pixels, 0, 3);
        ConvolveOp operation = new ConvolveOp(new Kernel(3, 1, new float[]{.25f, .5f, .25f}),
                ConvolveOp.EDGE_NO_OP, null);
        Image image = Toolkit.getDefaultToolkit().createImage(
                new FilteredImageSource(source, new BufferedImageFilter(operation)));
        PixelGrabber grabber = new PixelGrabber(image, 0, 0, -1, -1, true);
        if (!grabber.grabPixels(10000) || grabber.getWidth() != 3 || grabber.getHeight() != 1)
            throw new AssertionError("Filtered image production failed");
        int[] result = (int[]) grabber.getPixels();
        if (result[0] != pixels[0] || result[1] != pixels[1] || result[2] != pixels[2])
            throw new AssertionError("Filtered producer pixels");
        // Also force the ToolkitImage's ImageRepresentation path, used by JSVG.
        MediaTracker tracker = new MediaTracker(new Canvas());
        tracker.addImage(image, 1);
        tracker.waitForID(1, 10000);
        if (tracker.isErrorID(1) || image.getWidth(null) != 3 || image.getHeight(null) != 1)
            throw new AssertionError("Toolkit image representation failed");
        System.out.println("Filtered image production, pixel grabbing, and toolkit representation passed");
    }
}
