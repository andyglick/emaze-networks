package net.emaze.networks.ipv6;

import java.io.Serializable;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.networks.FixedSizeNatural;

public class Ipv6 implements Comparable<Ipv6>, Serializable {

    private static final int BIT_SIZE = 128;
    private static final FixedSizeNatural FIRST_ADDRESS = FixedSizeNatural.zero(BIT_SIZE);
    private static final FixedSizeNatural LAST_ADDRESS = FixedSizeNatural.biggest(BIT_SIZE);
    protected final FixedSizeNatural address;

    public Ipv6(FixedSizeNatural address) {
        this.address = address;
    }

    public static Ipv6 parse(String ip) {
        dbc.precondition(ip != null, "Cannot parse a null ip address");
        final FixedSizeNatural bits = FixedSizeNatural.fromByteArray(new Ipv6ToByteArray().apply(ip));
        return new Ipv6(bits);
    }

    public static Ipv6 fromBits(int... bits) {
        dbc.precondition(bits.length == 4, "An IPv6 must be built with 128 bits");
        return new Ipv6(new FixedSizeNatural(bits, bits.length * Integer.SIZE));
    }

    public static Ipv6 getFirstIp() {
        return new Ipv6(FIRST_ADDRESS);
    }

    public static Ipv6 getLastIp() {
        return new Ipv6(LAST_ADDRESS);
    }

    public Ipv6 mask(Ipv6Mask mask) {
        return new Ipv6(address.and(mask.bits()));
    }

    public Ipv6 next() {
        return address.equals(LAST_ADDRESS) ? new Ipv6(LAST_ADDRESS) : new Ipv6(address.increment());
    }

    public Ipv6 previous() {
        return address.equals(FIRST_ADDRESS) ? new Ipv6(FIRST_ADDRESS) : new Ipv6(address.decrement());
    }

    @Override
    public int compareTo(Ipv6 other) {
        return address.compareTo(other.address);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Ipv6 == false) {
            return false;
        }
        final Ipv6 other = (Ipv6) obj;
        return address.equals(other.address);
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(address).toHashCode();
    }

    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder();
        final byte[] octets = address.toByteArray();
        builder.append(String.format("%02x%02x", octets[0] & 0xFF, octets[1] & 0xFF));
        for (int index = 2; index != octets.length; index += 2) {
            builder.append(String.format(":%02x%02x", octets[index] & 0xFF, octets[index + 1] & 0xFF));
        }
        return builder.toString();
    }

    public byte[] toByteArray() {
        return address.toByteArray();
    }

    public FixedSizeNatural bits() {
        return address;
    }

}
