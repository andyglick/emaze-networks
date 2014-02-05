package net.emaze.networks;

import java.math.BigInteger;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;

public class Ipv6Mask implements Comparable<Ipv6Mask> {

    private static final int BIT_SIZE = 128;
    protected final int size;

    public Ipv6Mask(int size) {
        this.size = size;
    }

    public static Ipv6Mask net(int mask) {
        dbc.precondition(mask >= 0 && mask <= BIT_SIZE, "mask must be between 0 and %s", BIT_SIZE);
        return new Ipv6Mask(mask);
    }

    public static Ipv6Mask host(int mask) {
        dbc.precondition(mask >= 0 && mask <= BIT_SIZE, "mask must be between 0 and %s", BIT_SIZE);
        return new Ipv6Mask(BIT_SIZE - mask);
    }

    public static Ipv6Mask getWidestMask() {
        return Ipv6Mask.net(0);
    }

    public static Ipv6Mask getNarrowestMask() {
        return Ipv6Mask.net(BIT_SIZE);
    }

    public FixedSizeNatural bits() {
        return FixedSizeNatural.biggest(BIT_SIZE).shiftLeft(BIT_SIZE - size);
    }

    public FixedSizeNatural hostBits() {
        return bits().not();
    }

    public BigInteger hosts() {
        return FixedSizeNatural.one(BIT_SIZE + 1).shiftLeft(BIT_SIZE - size).bigIntegerValue();
    }

    public int hostPopulation() {
        return BIT_SIZE - size;
    }

    public Ipv6Mask narrowHosts() {
        return size == BIT_SIZE ? new Ipv6Mask(BIT_SIZE) : new Ipv6Mask(size + 1);
    }

    public Ipv6Mask widenHosts() {
        return size == 0 ? new Ipv6Mask(0) : new Ipv6Mask(size - 1);
    }

    public boolean isNarrowest() {
        return size == BIT_SIZE;
    }

    public boolean isWidest() {
        return size == 0;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Ipv6Mask == false) {
            return false;
        }
        final Ipv6Mask other = (Ipv6Mask) obj;
        return this.size == other.size;
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(size).toHashCode();
    }

    @Override
    public int compareTo(Ipv6Mask other) {
        return Integer.compare(this.size, other.size);
    }

    @Override
    public String toString() {
        return Integer.toString(size);
    }

    public int population() {
        return size;
    }

}
