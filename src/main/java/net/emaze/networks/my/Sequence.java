package net.emaze.networks.my;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import net.emaze.dysfunctional.Applications;
import net.emaze.dysfunctional.Strings;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.iterations.ReadOnlyIterator;

public class Sequence implements Iterable<Boolean> {

    private final int[] internal;
    private final int length;

    public Sequence(int[] internal, int lengthInBits) {
        dbc.precondition(internal.length == lengthOfIntContainer(lengthInBits), "Internal representation must be aligned to lengthInBits");
        dbc.precondition(internal.length > 0, "Cannot create a sequence with empty data");
        dbc.precondition((lengthInBits % 32) == 0 || (internal[0] & (0xFFFFFFFF << (lengthInBits % 32))) == 0, "Unused bits contain data");
        this.internal = internal;
        this.length = lengthInBits;
    }

    public static Sequence fromByteArray(byte[] in) {
        final int[] out = new int[(in.length * Byte.SIZE) / Integer.SIZE + ((in.length * Byte.SIZE) % Integer.SIZE > 0 ? 1 : 0)];
        final int remaining = in.length % 4;
        final ByteBuffer buffer = ByteBuffer.wrap(new byte[in.length + (remaining > 0 ? 4 - remaining : 0)]);
        buffer.put(new byte[remaining > 0 ? 4 - remaining : 0]);
        buffer.put(in);
        buffer.flip();
        buffer.asIntBuffer().get(out);
        return new Sequence(out, in.length * Byte.SIZE);
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

    public Sequence shiftLeft(int shift) {
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
        return new Sequence(clearExcess(shifted, length), length);
    }

    public Sequence shiftRight(int shift) {
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
        return new Sequence(clearExcess(shifted, length), length);
    }

    public Sequence not() {
        final int[] negation = new int[internal.length];
        for (int index = 0; index != internal.length; ++index) {
            negation[index] = ~internal[index];
        }
        return new Sequence(clearExcess(negation, length), length);
    }

    public Sequence and(Sequence other) {
        dbc.precondition(this.length == other.length, "Sequences length is different");
        final int[] conjunction = new int[internal.length];
        for (int index = 0; index != internal.length; ++index) {
            conjunction[index] = this.internal[index] & other.internal[index];
        }
        return new Sequence(clearExcess(conjunction, length), length);
    }

    public Sequence or(Sequence other) {
        dbc.precondition(this.length == other.length, "Sequences length is different");
        final int[] inclusiveDisjunction = new int[internal.length];
        for (int index = 0; index != internal.length; ++index) {
            inclusiveDisjunction[index] = this.internal[index] | other.internal[index];
        }
        return new Sequence(clearExcess(inclusiveDisjunction, length), length);
    }

    public Sequence xor(Sequence other) {
        dbc.precondition(this.length == other.length, "Sequences length is different");
        final int[] exclusiveDisjunction = new int[internal.length];
        for (int index = 0; index != internal.length; ++index) {
            exclusiveDisjunction[index] = this.internal[index] ^ other.internal[index];
        }
        return new Sequence(clearExcess(exclusiveDisjunction, length), length);
    }

    public Sequence increment() {
        if (this.isLast()) {
            return this;
        }
        final int[] incremented = new int[internal.length];
        int carry = 1;
        for (int index = internal.length - 1; index >= 0 && carry > 0; --index) {
            incremented[index] = internal[index] + carry;
            carry = internal[index] == 0xFFFFFFFF ? 1 : 0;
        }
        return new Sequence(clearExcess(incremented, length), length);
    }

    public Sequence decrement() {
        if (this.isFirst()) {
            return this;
        }
        final int[] decremented = new int[internal.length];
        int carry = -1;
        for (int index = internal.length - 1; index >= 0 && carry < 0; --index) {
            decremented[index] = internal[index] + carry;
            carry = internal[index] == 0 ? -1 : 0;
        }
        return new Sequence(clearExcess(decremented, length), length);
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

    public boolean isFirst() {
        return Arrays.equals(this.internal, first().internal);
    }

    public boolean isLast() {
        return Arrays.equals(this.internal, last().internal);
    }

    public Sequence first() {
        return new Sequence(new int[this.intsLength()], length);
    }

    public Sequence last() {
        return this.first().not();
    }

    public Sequence widen(int newLength) {
        dbc.precondition(newLength > length, "newLength shoul be greater than current length");
        final int[] wider = new int[lengthOfIntContainer(newLength)];
        System.arraycopy(internal, 0, wider, wider.length - internal.length, internal.length);
        return new Sequence(wider, newLength);
    }

    public Sequence narrow(int newLength) {
        dbc.precondition(newLength < length, "newLength shoul be lesser than current length");
        final int[] narrower = new int[lengthOfIntContainer(newLength)];
        System.arraycopy(internal, internal.length - narrower.length, narrower, 0, narrower.length);
        return new Sequence(narrower, newLength);
    }

    private int lengthOfIntContainer(int numBits) {
        return numBits / Integer.SIZE + (numBits % Integer.SIZE > 0 ? 1 : 0);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Sequence == false) {
            return false;
        }
        final Sequence other = (Sequence) obj;
        return new EqualsBuilder()
                .append(internal, other.internal)
                .append(length, other.length)
                .isEquals();
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
        return Strings.join(Applications.transform(new SequenceIterator(this), new ToBitValue()));
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
        dbc.precondition((index >= 0) && (index < length), "index must be a value between 0 and %s", length);
        final int value = (internal[internal.length - 1 - index / 32] & 1 << (index % 32)) >>> index;
        return value > 0;
    }

    @Override
    public Iterator<Boolean> iterator() {
        return new SequenceIterator(this);
    }

    public int[] clearExcess(int[] bits, int lengthInBits) {
        final int[] copy = new int[bits.length];
        System.arraycopy(bits, 0, copy, 0, bits.length);
        final int bitsToClear = intsLength() * 32 - lengthInBits;
        final int intsToClear = bitsToClear / 32;
        final int remaining = bitsToClear - (bitsToClear / 32) * 32;
        int cleared = 0;
        while (cleared < intsToClear) {
            copy[cleared++] = 0;
        }
        copy[cleared] &= (0xFFFFFFFF >>> remaining);
        return copy;
    }

    public static class SequenceIterator extends ReadOnlyIterator<Boolean> {

        private final Sequence sequence;
        private int index;

        public SequenceIterator(Sequence sequence) {
            this.sequence = sequence;
            this.index = sequence.length() - 1;
        }

        @Override
        public boolean hasNext() {
            return index >= 0;
        }

        @Override
        public Boolean next() {
            return sequence.bit(index--);
        }
    }

    public static class ToBitValue implements Delegate<String, Boolean> {

        @Override
        public String perform(Boolean t) {
            return t ? "1" : "0";
        }

    }

}
