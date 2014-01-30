package net.emaze.networks.my;

import java.math.BigInteger;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;

public class MyIp implements Comparable<MyIp> {

    private final BigInteger address;
    private final IpPolicy policy;

    public MyIp(BigInteger address, IpPolicy policy) {
        dbc.precondition(Order.of(address.compareTo(policy.getLastIp().bits())).isLte(), "Ip number is out of range");
        this.address = address;
        this.policy = policy;
    }

    public IpPolicy version() {
        return policy;
    }

    public byte[] toByteArray() {
        return address.toByteArray();
    }

    public BigInteger bits() {
        return address;
    }

    public MyIp mask(MyMask mask) {
        return new MyIp(address.and(mask.bits()), policy);
    }

    public MyIp next() {
        return address.equals(policy.maxValue()) ? policy.getLastIp() : new MyIp(address.add(BigInteger.ONE), policy);
    }

    public MyIp previous() {
        return address.equals(policy.minValue()) ? policy.getFirstIp() : new MyIp(address.subtract(BigInteger.ONE), policy);
    }

    @Override
    public int compareTo(MyIp other) {
        return policy.selectForComparison(other.policy).compare(this, other);
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(address).toHashCode();
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof MyIp == false) {
            return false;
        }
        final MyIp other = (MyIp) object;
        return Order.of(policy.selectForComparison(other.policy).compare(this, other)) == Order.EQ;
    }
}
