package net.emaze.networks.my;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;

public class Mask implements Comparable<Mask> {

    private final int size;
    final IpPolicy policy;

    public Mask(int size, IpPolicy policy) {
        dbc.precondition(size <= policy.maxPopulation(), "mask exceed maximum size");
        this.size = size;
        this.policy = policy;
    }

    public IpPolicy version() {
        return policy;
    }

    public FixedSizeNatural bits() {
        final FixedSizeNatural fullMask = policy.maxValue();
        return fullMask.shiftLeft(policy.maxPopulation() - size).and(fullMask);
    }

    public FixedSizeNatural hostBits() {
        return bits().xor(policy.maxValue());
    }

    public int population() {
        return size;
    }

    public FixedSizeNatural hosts() {
        return FixedSizeNatural.one(policy.maxPopulation()).shiftLeft(policy.maxPopulation() - size);
    }

    public Mask narrowHosts() {
        return size == policy.maxPopulation() ? policy.getNarrowestMask() : new Mask(size + 1, policy);
    }

    public Mask widenHosts() {
        return size == 0 ? policy.getWidestMask() : new Mask(size - 1, policy);
    }

    public boolean isNarrowest() {
        return this.equals(policy.getNarrowestMask());
    }

    public boolean isWidest() {
        return this.equals(policy.getWidestMask());
    }

    @Override
    public int compareTo(Mask other) {
        return policy.selectForComparison(other.policy).compare(this, other);
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(size).toHashCode();
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Mask == false) {
            return false;
        }
        final Mask other = (Mask) object;
        return Order.of(policy.selectForComparison(other.policy).compare(this, other)) == Order.EQ;
    }
}
