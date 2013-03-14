package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.CompareToBuilder;

public class Mask implements Comparable<Mask> {

    public static final Mask NARROWEST = new Mask(0xffffffff);
    public static final Mask WIDEST = new Mask(0x00000000);
    private final int bits;

    private Mask(int bits) {
        this.bits = bits;
    }

    public static Mask parse(String dottedNetmask) {
        final int bits = new DottedOctetFormToLong().perform(dottedNetmask).intValue();
        int population = Integer.bitCount(bits);
        int expectedNumberOfContiguousZeros = Integer.signum(bits) < 0 ? Integer.numberOfTrailingZeros(bits) : Integer.numberOfLeadingZeros(bits);
        dbc.precondition(expectedNumberOfContiguousZeros == (32 - population), "malformed mask");
        return new Mask(bits);
    }

    public static Mask host(int population) {
        //TODO pre 0 <= pop <= 32  or return widest/narrowest when over?
        if (population == 0) {
            return WIDEST;
        }
        return new Mask(0xffffffff >>> (32 - population));
    }

    public static Mask net(int population) {
        //TODO pre 0 <= pop <= 32  or return widest/narrowest when over?
        if (population == 0) {
            return WIDEST;
        }
        return new Mask(0xffffffff << (32 - population));
    }

    public int bits() {
        return bits;
    }

    public int population() {
        return Integer.bitCount(bits);
    }

    @Override
    public String toString() {
        return new IntToDottedOctetForm().perform(bits);
    }

    public Mask narrowHosts() {
        if (bits == 0) {
            return new Mask(0x80000000);
        }
        return new Mask((bits >> 1));
    }

    public Mask widenHosts() {
        return new Mask(bits << 1);
    }

    public boolean isNetmask() {
        return bits == 0 ? true : Integer.highestOneBit(bits) == 0x80000000;
    }

    public boolean isHostmask() {
        return bits == 0 ? true : Integer.lowestOneBit(bits) == 0x00000001;
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Mask == false) {
            return false;
        }
        final Mask netmask = (Mask) other;
        return new EqualsBuilder().append(this.bits, netmask.bits).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(bits).toHashCode();
    }

    @Override
    public int compareTo(Mask other) {
        return new CompareToBuilder().append(this.bits, other.bits).toComparison();
    }
}
