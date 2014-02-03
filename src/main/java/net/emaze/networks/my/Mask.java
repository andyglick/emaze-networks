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

    public static Mask netV4(String mask) {
        dbc.precondition(mask != null, "mask must be not-null");
        final IpPolicy policy = new IpPolicy.V4();
        if (mask.contains(".")) {
            final FixedSizeNatural bits = Ip.parseV4(mask).bits();
            final FixedSizeNatural other = FixedSizeNatural.biggest(32).shiftRight(bits.bitCount());
            dbc.precondition(bits.and(other).equals(FixedSizeNatural.zero(32)), "Malformed mask");
            return new Mask(bits.bitCount(), policy);
        }
        throw new IllegalArgumentException("Not an IPv4 mask");
    }

    public static Mask netV4(int mask) {
        return new Mask(mask, new IpPolicy.V4());
    }

    public static Mask netV6(int mask) {
        return new Mask(mask, new IpPolicy.V6());
    }

    public static Mask hostV4(String mask) {
        dbc.precondition(mask != null, "mask must be not-null");
        final IpPolicy policy = new IpPolicy.V4();
        if (mask.contains(".")) {
            final FixedSizeNatural bits = Ip.parseV4(mask).bits().not();
            final FixedSizeNatural other = FixedSizeNatural.biggest(32).shiftRight(bits.bitCount());
            dbc.precondition(bits.and(other).equals(FixedSizeNatural.zero(32)), "Malformed mask");
            return new Mask(bits.bitCount(), policy);
        }
        throw new IllegalArgumentException("Not an IPv4 mask");
    }

    public static Mask hostV4(int mask) {
        final IpPolicy policy = new IpPolicy.V4();
        return new Mask(policy.maxPopulation() - mask, policy);
    }

    public static Mask hostV6(int mask) {
        final IpPolicy policy = new IpPolicy.V6();
        return new Mask(policy.maxPopulation() - mask, policy);
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

    public int hostPopulation() {
        return policy.maxPopulation() - size;
    }

    public FixedSizeNatural hosts() {
        return FixedSizeNatural.one(policy.maxPopulation() + 1).shiftLeft(policy.maxPopulation() - size);
    }

    public Mask narrowHosts() {
        return size == policy.maxPopulation() ? policy.getNarrowestMask() : new Mask(size + 1, policy);
    }

    public Mask widenHosts() {
        return size == 0 ? policy.getWidestMask() : new Mask(size - 1, policy);
    }

    public boolean isNarrowest() {
        return size == policy.maxPopulation();
    }

    public boolean isWidest() {
        return size == 0;
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

    @Override
    public String toString() {
        return String.format("/%s", size);
    }
    
    
}
