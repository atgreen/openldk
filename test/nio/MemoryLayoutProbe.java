// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.foreign.*;
import java.lang.invoke.*;

public class MemoryLayoutProbe {
    public static void main(String[] args) {
        int[] values = {11, 22, 33};
        MemorySegment heap = MemorySegment.ofArray(values);
        VarHandle fixed = MethodHandles.insertCoordinates(ValueLayout.JAVA_INT.varHandle(), 1, 4L)
                                       .withInvokeExactBehavior();
        if ((int) fixed.get(heap) != 22 || (int) fixed.getAcquire(heap) != 22)
            throw new AssertionError("bound coordinates");
        fixed.set(heap, 44);
        if (values[1] != 44 || !fixed.compareAndSet(heap, 44, 55) || values[1] != 55)
            throw new AssertionError("heap segment update");
        try {
            fixed.set(heap.asReadOnly(), 1);
            throw new AssertionError("read-only write accepted");
        } catch (IllegalArgumentException expected) { }
        StructLayout layout = MemoryLayout.structLayout(ValueLayout.JAVA_FLOAT.withName("x"),
                                                         ValueLayout.JAVA_FLOAT.withName("y"));
        VarHandle y = MethodHandles.insertCoordinates(layout.varHandle(MemoryLayout.PathElement.groupElement("y")), 1, 0L);
        Arena arena = Arena.ofConfined();
        MemorySegment segment = arena.allocate(layout);
        y.set(segment, -12.5f);
        if ((float) y.get(segment) != -12.5f || segment.get(ValueLayout.JAVA_FLOAT, 4) != -12.5f)
            throw new AssertionError("native struct field");
        arena.close();
        try {
            y.get(segment);
            throw new AssertionError("closed arena access accepted");
        } catch (IllegalStateException expected) { }
        System.out.println("Memory-layout VarHandles, bound coordinates, atomic access, and segment checks passed");
    }
}
