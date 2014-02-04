package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;

public class Ip implements Comparable<Ip> {

    private final FixedSizeNatural address;
    private final IpPolicy policy;

    public Ip(FixedSizeNatural address, IpPolicy policy) {
        dbc.precondition(policy.acceptSize(address), "Policy does not accept address");
        this.address = address;
        this.policy = policy;
    }

    public static Ip parse(String ip) {
        dbc.precondition(ip != null, "address must be not-null");
        if (ip.contains(":")) {
            return parseV6(ip);
        }
        return parseV4(ip);
    }

    public static Ip parseV4(String dottedIpAddress) {
        dbc.precondition(dottedIpAddress != null, "address must be not-null");
        final byte[] octets = new Ipv4DottedOctetFormToByteArray().perform(dottedIpAddress);
        final IpPolicy.V4 v4 = new IpPolicy.V4();
        return new Ip(FixedSizeNatural.fromByteArray(octets), v4);
    }

    public static Ip parseV6(String ip) {
        dbc.precondition(ip != null, "address must be not-null");
        final FixedSizeNatural bits = FixedSizeNatural.fromByteArray(new Ipv6ToByteArray().perform(ip));
        return new Ip(bits, new IpPolicy.V6());
    }

    public static Ip fromBits(int... bits) {
        dbc.precondition(bits.length == 1 || bits.length == 4, "A new IP must be built with either 32 or 128 bits");
        return new Ip(new FixedSizeNatural(bits, bits.length * Integer.SIZE), bits.length == 1 ? new IpPolicy.V4() : new IpPolicy.V6());
    }

    public IpPolicy version() {
        return policy;
    }

    public byte[] toByteArray() {
        return address.toByteArray();
    }

    public FixedSizeNatural bits() {
        return address;
    }

    public Ip mask(Mask mask) {
        dbc.precondition(mask != null, "Cannot mask using a null value");
        return new Ip(address.and(mask.bits()), policy);
    }

    public Ip next() {
        return address.equals(policy.maxValue()) ? policy.getLastIp() : new Ip(address.increment(), policy);
    }

    public Ip previous() {
        return address.equals(policy.minValue()) ? policy.getFirstIp() : new Ip(address.decrement(), policy);
    }

    @Override
    public int compareTo(Ip other) {
        dbc.precondition(other != null, "Cannot compare with a null ip"); //FIXME: Interface says: throw a NPE...
        return policy.selectForComparison(other.policy).compare(this, other);
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(address).toHashCode();
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Ip == false) {
            return false;
        }
        final Ip other = (Ip) object;
        return Order.of(policy.selectForComparison(other.policy).compare(this, other)) == Order.EQ;
    }

    @Override
    public String toString() {
        return policy.toCanonicalString(address);
    }
    
    
}
