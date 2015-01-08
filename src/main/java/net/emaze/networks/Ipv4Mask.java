package net.emaze.networks;

import java.io.Serializable;
import java.math.BigInteger;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;

public class Ipv4Mask implements Comparable<Ipv4Mask>, Serializable {

    private static final int BIT_SIZE = 32;
    protected final int size;

    public Ipv4Mask(int size) {
        this.size = size;
    }

    public static Ipv4Mask net(String mask) {
        dbc.precondition(mask != null, "mask must be not-null");
        if (mask.contains(".")) {
            final FixedSizeNatural bits = Ipv4.parse(mask).bits();
            final FixedSizeNatural other = FixedSizeNatural.biggest(BIT_SIZE).shiftRight(bits.bitCount());
            dbc.precondition(bits.and(other).equals(FixedSizeNatural.zero(BIT_SIZE)), "Malformed mask");
            return new Ipv4Mask(bits.bitCount());
        }
        throw new IllegalArgumentException("Not an IPv4 mask");
    }

    public static Ipv4Mask net(int mask) {
        dbc.precondition(mask >= 0 && mask <= BIT_SIZE, "mask must be between 0 and %s", BIT_SIZE);
        return new Ipv4Mask(mask);
    }

    public static Ipv4Mask host(String mask) {
        dbc.precondition(mask != null, "mask must be not-null");
        if (mask.contains(".")) {
            final FixedSizeNatural bits = Ipv4.parse(mask).bits().not();
            final FixedSizeNatural other = FixedSizeNatural.biggest(BIT_SIZE).shiftRight(bits.bitCount());
            dbc.precondition(bits.and(other).equals(FixedSizeNatural.zero(BIT_SIZE)), "Malformed mask");
            return new Ipv4Mask(bits.bitCount());
        }
        throw new IllegalArgumentException("Not an IPv4 mask");
    }

    public static Ipv4Mask host(int mask) {
        dbc.precondition(mask >= 0 && mask <= BIT_SIZE, "mask must be between 0 and %s", BIT_SIZE);
        return new Ipv4Mask(BIT_SIZE - mask);
    }

    public static Ipv4Mask getWidestMask() {
        return Ipv4Mask.net(0);
    }

    public static Ipv4Mask getNarrowestMask() {
        return Ipv4Mask.net(BIT_SIZE);
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

    public Ipv4Mask narrowHosts() {
        return size == BIT_SIZE ? new Ipv4Mask(BIT_SIZE) : new Ipv4Mask(size + 1);
    }

    public Ipv4Mask widenHosts() {
        return size == 0 ? new Ipv4Mask(0) : new Ipv4Mask(size - 1);
    }

    public boolean isNarrowest() {
        return size == BIT_SIZE;
    }

    public boolean isWidest() {
        return size == 0;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Ipv4Mask == false) {
            return false;
        }
        final Ipv4Mask other = (Ipv4Mask) obj;
        return this.size == other.size;
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(size).toHashCode();
    }

    @Override
    public int compareTo(Ipv4Mask other) {
        return Integer.compare(this.size, other.size);
    }

    @Override
    public String toString() {
        return new Ipv4(bits()).toString();
    }

    public int population() {
        return size;
    }

}
