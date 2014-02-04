package net.emaze.networks;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import net.emaze.dysfunctional.Strings;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;

public class FixedSizeNatural implements Comparable<FixedSizeNatural> {

    private final long COMPARISON_MASK = 0xFFFFFFFFL;
    private final int[] internal;
    private final int length;

    public FixedSizeNatural(int[] internal, int lengthInBits) {
        dbc.precondition(internal.length == lengthOfIntContainer(lengthInBits), "Internal representation must be aligned to lengthInBits");
        dbc.precondition(internal.length > 0, "Cannot create a fixed size natural with empty data");
        dbc.precondition((lengthInBits % 32) == 0 || (internal[0] & (0xFFFFFFFF << (lengthInBits % 32))) == 0, "Unused bits contain data");
        this.internal = internal;
        this.length = lengthInBits;
    }

    public static FixedSizeNatural of(int... value) {
        return new FixedSizeNatural(value, Integer.SIZE * value.length);
    }

    public static FixedSizeNatural of(long... value) {
        final int[] buf = new int[value.length * 2];
        for (int index = 0; index != value.length; ++index) {
            buf[index * 2] = (int) (value[index] >>> 32);
            buf[index * 2 + 1] = (int) value[index];
        }
        return new FixedSizeNatural(buf, Long.SIZE * value.length);
    }

    public static FixedSizeNatural zero(int length) {
        return new FixedSizeNatural(new int[lengthOfIntContainer(length)], length);
    }

    public static FixedSizeNatural one(int length) {
        final int[] internal = new int[lengthOfIntContainer(length)];
        internal[internal.length - 1] = 1;
        return new FixedSizeNatural(internal, length);
    }

    public static FixedSizeNatural biggest(int length) {
        final int[] internal = new int[lengthOfIntContainer(length)];
        Arrays.fill(internal, 0xFFFFFFFF);
        return new FixedSizeNatural(clearExcess(internal, length), length);
    }

    public static FixedSizeNatural fromByteArray(byte[] in) {
        final int[] out = new int[(in.length * Byte.SIZE) / Integer.SIZE + ((in.length * Byte.SIZE) % Integer.SIZE > 0 ? 1 : 0)];
        final int remaining = in.length % 4;
        final ByteBuffer buffer = ByteBuffer.wrap(new byte[in.length + (remaining > 0 ? 4 - remaining : 0)]);
        buffer.put(new byte[remaining > 0 ? 4 - remaining : 0]);
        buffer.put(in);
        buffer.flip();
        buffer.asIntBuffer().get(out);
        return new FixedSizeNatural(out, in.length * Byte.SIZE);
    }

    public byte[] toByteArray() {
        final int intsToBytes = intsLength() * 4;
        final ByteBuffer buffer = ByteBuffer.wrap(new byte[intsToBytes]);
        buffer.asIntBuffer().put(internal);
        final byte[] out = new byte[bytesLength()];
        buffer.position(intsToBytes - bytesLength());
        buffer.get(out);
        return out;

    }

    public FixedSizeNatural shiftLeft(int shift) {
        dbc.precondition(shift >= 0, "cannot shift for negative values");
        if (shift == 0) {
            return this;
        }
        final int chunksToShift = shift >>> 5;
        final int bitsToShift = shift & 0x1f;
        if (chunksToShift >= internal.length) {
            return this.first();
        }
        final int[] shifted = new int[internal.length];
        if (bitsToShift == 0) {
            System.arraycopy(internal, chunksToShift, shifted, 0, internal.length - chunksToShift);
        } else {
            final int carryBitsRightShift = 32 - bitsToShift;
            int i = 0;
            int j = chunksToShift;
            while (j < internal.length - 1) {
                shifted[i] = internal[j] << bitsToShift;
                j++;
                shifted[i] |= internal[j] >>> carryBitsRightShift;
                i++;
            }
            shifted[i] = internal[j] << bitsToShift;
        }
        return new FixedSizeNatural(clearExcess(shifted, length), length);
    }

    public FixedSizeNatural shiftRight(int shift) {
        dbc.precondition(shift >= 0, "cannot shift for negative values");
        if (shift == 0) {
            return this;
        }
        int chunksToShift = shift >>> 5;
        int bitsToShift = shift & 0x1f;
        if (chunksToShift >= internal.length) {
            return this.first();
        }
        int[] shifted = new int[internal.length];
        if (bitsToShift == 0) {
            System.arraycopy(internal, 0, shifted, chunksToShift, internal.length - chunksToShift);
        } else {
            int carryBitsLeftShift = 32 - bitsToShift;
            int i = chunksToShift;
            int j = 0;
            shifted[i] |= internal[j] >>> bitsToShift;
            i++;
            while (i < internal.length) {
                shifted[i] = internal[j] << carryBitsLeftShift;
                j++;
                shifted[i] |= internal[j] >>> bitsToShift;
                i++;
            }
        }
        return new FixedSizeNatural(clearExcess(shifted, length), length);
    }

    public FixedSizeNatural not() {
        final int[] negation = new int[internal.length];
        for (int index = 0; index != internal.length; ++index) {
            negation[index] = ~internal[index];
        }
        return new FixedSizeNatural(clearExcess(negation, length), length);
    }

    public FixedSizeNatural and(FixedSizeNatural other) {
        dbc.precondition(this.length == other.length, "FixedSizeNatural lengths are different");
        final int[] conjunction = new int[internal.length];
        for (int index = 0; index != internal.length; ++index) {
            conjunction[index] = this.internal[index] & other.internal[index];
        }
        return new FixedSizeNatural(conjunction, length);
    }

    public FixedSizeNatural or(FixedSizeNatural other) {
        dbc.precondition(this.length == other.length, "FixedSizeNatural lengths are different");
        final int[] inclusiveDisjunction = new int[internal.length];
        for (int index = 0; index != internal.length; ++index) {
            inclusiveDisjunction[index] = this.internal[index] | other.internal[index];
        }
        return new FixedSizeNatural(inclusiveDisjunction, length);
    }

    public FixedSizeNatural xor(FixedSizeNatural other) {
        dbc.precondition(this.length == other.length, "FixedSizeNatural lengths are different");
        final int[] exclusiveDisjunction = new int[internal.length];
        for (int index = 0; index != internal.length; ++index) {
            exclusiveDisjunction[index] = this.internal[index] ^ other.internal[index];
        }
        return new FixedSizeNatural(exclusiveDisjunction, length);
    }

    public FixedSizeNatural increment() {
        if (this.isLast()) {
            return this;
        }
        final int[] incremented = new int[internal.length];
        int carry = 1;
        for (int index = internal.length - 1; index >= 0; --index) {
            incremented[index] = internal[index] + carry;
            carry = carry != 0 && internal[index] == 0xFFFFFFFF ? 1 : 0;
        }
        return new FixedSizeNatural(clearExcess(incremented, length), length);
    }

    public FixedSizeNatural decrement() {
        if (this.isFirst()) {
            return this;
        }
        final int[] decremented = new int[internal.length];
        int borrow = -1;
        for (int index = internal.length - 1; index >= 0; --index) {
            decremented[index] = internal[index] + borrow;
            borrow = borrow != 0 && internal[index] == 0 ? -1 : 0;
        }
        return new FixedSizeNatural(clearExcess(decremented, length), length);
    }

    public int length() {
        return length;
    }

    public int bytesLength() {
        return length / 8 + (length % 8 > 0 ? 1 : 0);
    }

    public int intsLength() {
        return lengthOfIntContainer(length);
    }

    public int bitCount() {
        int bits = 0;
        for (int chunk : internal) {
            bits += Integer.bitCount(chunk);
        }
        return bits;
    }

    public boolean isFirst() {
        return Arrays.equals(this.internal, first().internal);
    }

    public boolean isLast() {
        return Arrays.equals(this.internal, last().internal);
    }

    public FixedSizeNatural first() {
        return new FixedSizeNatural(new int[this.intsLength()], length);
    }

    public FixedSizeNatural last() {
        return this.first().not();
    }

    public FixedSizeNatural extendTo(int newLength) {
        dbc.precondition(newLength > length, "new length should be greater than current length");
        final int[] wider = new int[lengthOfIntContainer(newLength)];
        System.arraycopy(internal, 0, wider, wider.length - internal.length, internal.length);
        return new FixedSizeNatural(wider, newLength);
    }

    public FixedSizeNatural truncateTo(int newLength) {
        dbc.precondition(newLength < length, "new length should be less than current length");
        final int[] narrower = new int[lengthOfIntContainer(newLength)];
        System.arraycopy(internal, internal.length - narrower.length, narrower, 0, narrower.length);
        return new FixedSizeNatural(clearExcess(narrower, newLength), newLength);
    }

    private static int lengthOfIntContainer(int numBits) {
        return numBits / Integer.SIZE + (numBits % Integer.SIZE > 0 ? 1 : 0);
    }

    public int intValue() {
        return internal[internal.length - 1];
    }

    public long longValue() {
        if (internal.length == 1) {
            return internal[0] & 0xFFFFFFFFFFFFFFFFL;
        }
        final long high = internal[internal.length - 2] & 0xFFFFFFFFFFFFFFFFL;
        final int low = internal[internal.length - 1];
        return (high << 32) | low;
    }
    
    public BigInteger bigIntegerValue() {
        return new BigInteger(1, this.toByteArray());
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof FixedSizeNatural == false) {
            return false;
        }
        final FixedSizeNatural other = (FixedSizeNatural) obj;
        return Order.of(this.compareTo(other)).isEq();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder()
                .append(internal)
                .append(length)
                .toHashCode();
    }

    @Override
    public String toString() {
        final String firstChunk = Integer.toBinaryString(internal[0]);
        final int remainingBits = length % Integer.SIZE;
        final StringBuilder stringBuilder = new StringBuilder();
        appendZeroes(remainingBits - firstChunk.length(), stringBuilder).append(firstChunk);
        for (int i = 1; i < internal.length; i++) {
            final String chunk = Integer.toBinaryString(internal[i]);
            appendZeroes(Integer.SIZE - chunk.length(), stringBuilder).append(chunk);
        }
        return stringBuilder.toString();
    }

    private StringBuilder appendZeroes(int times, StringBuilder builder) {
        for (int i = 0; i < times; i++) {
            builder.append('0');
        }
        return builder;
    }

    public String toString(int chunkSize) {
        final String canonical = this.toString();
        final List<String> chunks = new ArrayList<>();
        int index = canonical.length();
        while (index > 0) {
            chunks.add(0, canonical.substring(Math.max(index - chunkSize, 0), index));
            index -= chunkSize;
        }
        return Strings.interpose(chunks, " ");
    }

    public boolean bit(int index) {
        dbc.precondition((index >= 0) && (index < length), "index must be a value in [0, %s)", length);
        final int value = (internal[internal.length - 1 - index / 32] & 1 << (index % 32)) >>> index;
        return value > 0;
    }

    private static int[] clearExcess(int[] bits, int lengthInBits) {
        final int[] copy = new int[bits.length];
        System.arraycopy(bits, 0, copy, 0, bits.length);
        final int bitsToClear = lengthOfIntContainer(lengthInBits) * 32 - lengthInBits;
        final int intsToClear = bitsToClear / 32;
        final int remaining = bitsToClear - (bitsToClear / 32) * 32;
        int cleared = 0;
        while (cleared < intsToClear) {
            copy[cleared++] = 0;
        }
        copy[cleared] &= (0xFFFFFFFF >>> remaining);
        return copy;
    }

    @Override
    public int compareTo(FixedSizeNatural other) {
        final int maxLength = this.length >= other.length ? this.length : other.length;
        final FixedSizeNatural lhs = this.length == maxLength ? this : this.extendTo(maxLength);
        final FixedSizeNatural rhs = other.length == maxLength ? other : other.extendTo(maxLength);
        for (int i = 0; i < lhs.internal.length; i++) {
            if (lhs.internal[i] != rhs.internal[i]) {
                return (lhs.internal[i] & COMPARISON_MASK) < (rhs.internal[i] & COMPARISON_MASK) ? -1 : 1;
            }
        }
        return 0;
    }
}
