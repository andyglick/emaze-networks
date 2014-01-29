package net.emaze.networks.my;

import java.math.BigInteger;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;

public class MyMask implements Comparable<MyMask> {

    private final BigInteger mask;
    final IpPolicy policy;

    public MyMask(BigInteger mask, IpPolicy policy) {
        this.mask = mask;
        this.policy = policy;
    }
    
    public IpPolicy version() {
        return policy;
    }

    public BigInteger bits() {
        return mask;
    }

    public BigInteger hostBits() {
        return mask.xor(policy.maxValue());
    }

    public int population() {
        return mask.bitCount();
    }

    public BigInteger hosts() {
        return BigInteger.ONE.shiftLeft(policy.maxPopulation() - population());
    }

    public MyMask narrowHosts() {
        return mask.bitCount() == policy.maxPopulation() ? policy.getNarrowestMask() : new MyMask(mask.add(BigInteger.ONE), policy);
    }

    public MyMask widenHosts() {
        return mask.bitCount() == 0 ? policy.getWidestMask() : new MyMask(mask.subtract(BigInteger.ONE), policy);
    }

    public boolean isHostmask() {
        return mask.equals(BigInteger.ZERO) ? true : bits().getLowestSetBit() == 0;
    }

    public boolean isNetmask() {
        return mask.equals(BigInteger.ZERO) ? true : bits().bitLength() == policy.maxPopulation();
    }
    
    public boolean isNarrowest() {
        return mask.equals(policy.getNarrowestMask());
    }
    
    public boolean isWidest() {
        return mask.equals(policy.getWidestMask());
    }

    @Override
    public int compareTo(MyMask other) {
        return policy.selectForComparison(other.policy).compare(this, other);
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(mask).toHashCode();
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
