package net.emaze.networks.my;

import java.math.BigInteger;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;

public class MyMask implements Comparable<MyMask> {

    private final int size;
    final IpPolicy policy;

    public MyMask(int size, IpPolicy policy) {
        dbc.precondition(size <= policy.getNarrowestMask().population(), "mask exceed maximum size");
        this.size = size;
        this.policy = policy;
    }

    public IpPolicy version() {
        return policy;
    }

    public BigInteger bits() {
        final BigInteger fullMask = policy.getNarrowestMask().bits();
        return fullMask.shiftLeft(policy.maxPopulation() - size).and(fullMask);
    }

    public BigInteger hostBits() {
        return bits().xor(policy.maxValue());
    }

    public int population() {
        return size;
    }

    public BigInteger hosts() {
        return BigInteger.ONE.shiftLeft(policy.maxPopulation() - size);
    }

    public MyMask narrowHosts() {
        return size == policy.maxPopulation() ? policy.getNarrowestMask() : new MyMask(size + 1, policy);
    }

    public MyMask widenHosts() {
        return size == 0 ? policy.getWidestMask() : new MyMask(size - 1, policy);
    }

    public boolean isNarrowest() {
        return this.equals(policy.getNarrowestMask());
    }

    public boolean isWidest() {
        return this.equals(policy.getWidestMask());
    }

    @Override
    public int compareTo(MyMask other) {
        return policy.selectForComparison(other.policy).compare(this, other);
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(size).toHashCode();
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof MyMask == false) {
            return false;
        }
        final MyMask other = (MyMask) object;
        return Order.of(policy.selectForComparison(other.policy).compare(this, other)) == Order.EQ;
    }
}
