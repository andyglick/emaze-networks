package net.emaze.networks.my;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;

public class Ip implements Comparable<Ip> {

    private final FixedSizeNatural address;
    private final IpPolicy policy;

    public Ip(FixedSizeNatural address, IpPolicy policy) {
        dbc.precondition(Order.of(address.compareTo(policy.maxValue())).isLte(), "Ip number is out of range");
        this.address = address;
        this.policy = policy;
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
}
