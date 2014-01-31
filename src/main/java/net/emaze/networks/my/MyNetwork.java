package net.emaze.networks.my;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;
import net.emaze.dysfunctional.ranges.Range;
import net.emaze.dysfunctional.tuples.Pair;

public class MyNetwork {

    private final MyIp ip;
    private final MyMask mask;
    private final IpPolicy policy;

    public MyNetwork(MyIp ip, MyMask mask) {
        dbc.precondition(ip.version().equals(mask.version()), "version of ip and mask must be the same");
        this.ip = ip;
        this.mask = mask;
        this.policy = ip.version();
    }

    public IpPolicy version() {
        return policy;
    }

    public MyIp firstIp() {
        return ip;
    }

    public MyIp lastIp() {
        return new MyIp(ip.bits().or(mask.hostBits()), policy);
    }

    public MyMask netmask() {
        return mask;
    }

    public FixedSizeNatural size() {
        return mask.hosts();
    }

    public Pair<MyIp, MyMask> toCidr() {
        return Pair.of(ip, mask);
    }

    public Range<MyIp> toRange() {
        return policy.getRanges().closed(ip, lastIp());
    }

    public Pair<MyNetwork, MyNetwork> split() {
        dbc.precondition(mask.equals(policy.getNarrowestMask()), "Unsplittable CIDR");
        final MyMask halfMask = mask.narrowHosts();
        final MyNetwork first = new MyNetwork(ip, halfMask);
        final MyNetwork second = new MyNetwork(lastIp().mask(halfMask), halfMask);
        return Pair.of(first, second);
    }

    public boolean contains(MyIp ip) {
        if (Order.of(ip.compareTo(this.firstIp())) == Order.LT) {
            return false;
        }
        if (Order.of(ip.compareTo(this.lastIp())) == Order.GT) {
            return false;
        }
        return true;
    }

    public boolean contains(MyNetwork other) {
        final Order first = Order.of(this.firstIp().compareTo(other.firstIp()));
        final Order last = Order.of(this.lastIp().compareTo(other.lastIp()));
        return (first == Order.LT || first == Order.EQ) && (last == Order.EQ || last == Order.GT);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof MyNetwork == false) {
            return false;
        }
        final MyNetwork other = (MyNetwork) obj;
        return new EqualsBuilder().append(this.ip, other.ip).append(this.mask, other.mask).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(this.ip).append(this.mask).toHashCode();
    }

}
