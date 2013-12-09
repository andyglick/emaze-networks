package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.CompareToBuilder;

public class Ipv4Mask implements Comparable<Ipv4Mask> {

    public static final Ipv4Mask NARROWEST = new Ipv4Mask(0xffffffff);
    public static final Ipv4Mask WIDEST = new Ipv4Mask(0x00000000);
    private final int bits;

    private Ipv4Mask(int bits) {
        this.bits = bits;
    }

    public static Ipv4Mask parse(String dottedNetmask) {
        final int bits = new DottedOctetFormToLong().perform(dottedNetmask).intValue();
        int population = Integer.bitCount(bits);
        int expectedNumberOfContiguousZeros = Integer.signum(bits) < 0 ? Integer.numberOfTrailingZeros(bits) : Integer.numberOfLeadingZeros(bits);
        dbc.precondition(expectedNumberOfContiguousZeros == (32 - population), "malformed mask");
        return new Ipv4Mask(bits);
    }

    public static Ipv4Mask host(int population) {
        //TODO pre 0 <= pop <= 32  or return widest/narrowest when over?
        if (population == 0) {
            return WIDEST;
        }
        return new Ipv4Mask(0xffffffff >>> (32 - population));
    }

    public static Ipv4Mask net(int population) {
        //TODO pre 0 <= pop <= 32  or return widest/narrowest when over?
        if (population == 0) {
            return WIDEST;
        }
        return new Ipv4Mask(0xffffffff << (32 - population));
    }

    public int bits() {
        return bits;
    }

    public int[] octets() {
        final int[] o = new int[4];
        for (int i = 0; i != 4; ++i) {
            o[3 - i] = bits >> i * 8 & 0xff;
        }
        return o;
    }

    public int population() {
        return Integer.bitCount(bits);
    }

    @Override
    public String toString() {
        return new IntToDottedOctetForm().perform(bits);
    }

    public Ipv4Mask narrowHosts() {
        if (bits == 0) {
            return new Ipv4Mask(0x80000000);
        }
        return new Ipv4Mask((bits >> 1));
    }

    public Ipv4Mask widenHosts() {
        return new Ipv4Mask(bits << 1);
    }

    public boolean isNetmask() {
        return bits == 0 ? true : Integer.highestOneBit(bits) == 0x80000000;
    }

    public boolean isHostmask() {
        return bits == 0 ? true : Integer.lowestOneBit(bits) == 0x00000001;
    }

    public long hosts() {
        return 1L << (32 - this.population());
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Ipv4Mask == false) {
            return false;
        }
        final Ipv4Mask netmask = (Ipv4Mask) other;
        return new EqualsBuilder().append(this.bits, netmask.bits).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(bits).toHashCode();
    }

    @Override
    public int compareTo(Ipv4Mask other) {
        return new CompareToBuilder().append(this.bits, other.bits).toComparison();
    }
}
