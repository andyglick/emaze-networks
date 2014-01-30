package net.emaze.networks.my;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import net.emaze.dysfunctional.Applications;
import net.emaze.dysfunctional.Strings;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.iterations.ReadOnlyIterator;

public class Sequence implements Iterable<Boolean> {

    private final int[] internal;
    private final int length;

    public Sequence(int[] internal, int lengthInBits) {
        dbc.precondition(internal.length == (lengthInBits / 32 + (lengthInBits % 32 > 0 ? 1 : 0)), "Internal representation must be aligned to lengthInBits");
        this.internal = internal;
        this.length = lengthInBits;
    }

    public Sequence shiftLeft() {
        final int[] shifted = new int[internal.length];
        int carry = 0;
        for (int index = internal.length - 1; index >= 0; --index) {
            shifted[index] = (internal[index] << 1) | carry;
            carry = internal[index] >>> 31;
        }
        return new Sequence(clearExcess(shifted, length), length);
    }
    
    public Sequence shiftLeft(int howMuch) {
        dbc.precondition(howMuch > 0, "cannot shift for negative values");
        if (howMuch == 0) {
            return this;
        }
        Sequence shifted = this.shiftLeft();
        for (int times = 0; times != howMuch - 1; ++times) {
            shifted = shifted.shiftLeft();
        }
        return shifted;
    }

    public Sequence shiftRight() {
        final int[] shifted = new int[internal.length];
        int carry = 0;
        for (int index = 0; index != internal.length; ++index) {
            shifted[index] = (internal[index] >>> 1) | carry;
            carry = internal[index] & 1;
        }
        return new Sequence(clearExcess(shifted, length), length);
    }
    
    public Sequence shiftRight(int howMuch) {
        dbc.precondition(howMuch > 0, "cannot shift for negative values");
        if (howMuch == 0) {
            return this;
        }
        Sequence shifted = this.shiftRight();
        for (int times = 0; times != howMuch - 1; ++times) {
            shifted = shifted.shiftRight();
        }
        return shifted;
    }
    
    public Sequence not() {
        final int[] negation = new int[internal.length];
        for(int index = 0; index != internal.length; ++index) {
            negation[index] = ~internal[index];
        }
        return new Sequence(clearExcess(negation, length), length);
    }
    
    public Sequence and(Sequence other) {
        dbc.precondition(this.length == other.length, "Sequences length is different");
        final int[] conjunction = new int[internal.length];
        for(int index = 0; index != internal.length; ++index) {
            conjunction[index] = this.internal[index] & other.internal[index];
        }
        return new Sequence(clearExcess(conjunction, length), length);
    }
    
    public Sequence or(Sequence other) {
        dbc.precondition(this.length == other.length, "Sequences length is different");
        final int[] inclusiveDisjunction = new int[internal.length];
        for(int index = 0; index != internal.length; ++index) {
            inclusiveDisjunction[index] = this.internal[index] | other.internal[index];
        }
        return new Sequence(clearExcess(inclusiveDisjunction, length), length);
    }
    
    public Sequence xor(Sequence other) {
        dbc.precondition(this.length == other.length, "Sequences length is different");
        final int[] exclusiveDisjunction = new int[internal.length];
        for(int index = 0; index != internal.length; ++index) {
            exclusiveDisjunction[index] = this.internal[index] ^ other.internal[index];
        }
        return new Sequence(clearExcess(exclusiveDisjunction, length), length);
    }
    
    public int length() {
        return length;
    }

    public int bytesLength() {
        return length / 8 + (length % 8 > 0 ? 1 : 0);
    }

    public int intsLength() {
        return length / 32 + (length % 32 > 0 ? 1 : 0);
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
