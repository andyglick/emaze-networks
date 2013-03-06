package net.emaze.networks;

import java.util.Iterator;

public class LongBitsIterator implements Iterator<Boolean> {

    private final long value;
    private int index = 64;

    public LongBitsIterator(long value) {
        this.value = value;
    }

    @Override
    public boolean hasNext() {
        return index > 0;
    }

    @Override
    public Boolean next() {
        return (value & (1L << --index)) != 0;
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException("Not supported.");
    }
}
