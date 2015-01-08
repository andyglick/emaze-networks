package net.emaze.networks;

import java.io.Serializable;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;

public class Ipv4 implements Comparable<Ipv4>, Serializable {

    private static final int BIT_SIZE = 32;
    private static final FixedSizeNatural FIRST_ADDRESS = FixedSizeNatural.zero(BIT_SIZE);
    private static final FixedSizeNatural LAST_ADDRESS = FixedSizeNatural.biggest(BIT_SIZE);
    protected final FixedSizeNatural address;

    public Ipv4(FixedSizeNatural address) {
        this.address = address;
    }

    public static Ipv4 parse(String dottedIpAddress) {
        dbc.precondition(dottedIpAddress != null, "address must be not-null");
        final byte[] octets = new Ipv4DottedOctetFormToByteArray().perform(dottedIpAddress);
        return new Ipv4(FixedSizeNatural.fromByteArray(octets));
    }

    public static Ipv4 fromBits(int bits) {
        return new Ipv4(FixedSizeNatural.of(bits));
    }

    public static Ipv4 getFirstIp() {
        return new Ipv4(FIRST_ADDRESS);
    }

    public static Ipv4 getLastIp() {
        return new Ipv4(LAST_ADDRESS);
    }

    public Ipv4 mask(Ipv4Mask mask) {
        dbc.precondition(mask != null, "Cannot mask an IPv4 with a null netmask");
        return new Ipv4(address.and(mask.bits()));
    }

    public Ipv4 next() {
        return address.equals(LAST_ADDRESS) ? new Ipv4(LAST_ADDRESS) : new Ipv4(address.increment());
    }

    public Ipv4 previous() {
        return address.equals(FIRST_ADDRESS) ? new Ipv4(FIRST_ADDRESS) : new Ipv4(address.decrement());
    }

    @Override
    public int compareTo(Ipv4 other) {
        dbc.precondition(other != null, "Cannot compare a null Ipv4"); //THIS BREAKS Comparable
        return address.compareTo(other.address);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Ipv4 == false) {
            return false;
        }
        final Ipv4 other = (Ipv4) obj;
        return address.equals(other.address);
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(address).toHashCode();
    }

    @Override
    public String toString() {
        final byte[] octets = address.toByteArray();
        final int first = octets[0] & 0xFF;
        final int second = octets[1] & 0xFF;
        final int third = octets[2] & 0xFF;
        final int fourth = octets[3] & 0xFF;
        return String.format("%s.%s.%s.%s", first, second, third, fourth);
    }

    public byte[] toByteArray() {
        return address.toByteArray();
    }

    public FixedSizeNatural bits() {
        return address;
    }

}
