package net.emaze.networks.my;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;
import net.emaze.dysfunctional.ranges.Range;
import net.emaze.dysfunctional.tuples.Pair;

public class Network {

    private final Ip ip;
    private final Mask mask;
    private final IpPolicy policy;

    public Network(Ip ip, Mask mask) {
        dbc.precondition(ip.version().equals(mask.version()), "version of ip and mask must be the same");
        this.ip = ip;
        this.mask = mask;
        this.policy = ip.version();
    }

    public IpPolicy version() {
        return policy;
    }

    public Ip firstIp() {
        return ip;
    }

    public Ip lastIp() {
        return new Ip(ip.bits().or(mask.hostBits()), policy);
    }

    public Mask netmask() {
        return mask;
    }

    public FixedSizeNatural size() {
        return mask.hosts();
    }

    public Pair<Ip, Mask> toCidr() {
        return Pair.of(ip, mask);
    }

    public Range<Ip> toRange() {
        return policy.getRanges().closed(ip, lastIp());
    }

    public Pair<Network, Network> split() {
        dbc.precondition(mask.isNarrowest(), "Unsplittable CIDR");
        final Mask halfMask = mask.narrowHosts();
        final Network first = new Network(ip, halfMask);
        final Network second = new Network(lastIp().mask(halfMask), halfMask);
        return Pair.of(first, second);
    }

    public boolean contains(Ip ip) {
        if (Order.of(ip.compareTo(this.firstIp())) == Order.LT) {
            return false;
        }
        if (Order.of(ip.compareTo(this.lastIp())) == Order.GT) {
            return false;
        }
        return true;
    }

    public boolean contains(Network other) {
        final Order first = Order.of(this.firstIp().compareTo(other.firstIp()));
        final Order last = Order.of(this.lastIp().compareTo(other.lastIp()));
        return (first == Order.LT || first == Order.EQ) && (last == Order.EQ || last == Order.GT);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Network == false) {
            return false;
        }
        final Network other = (Network) obj;
        return new EqualsBuilder().append(this.ip, other.ip).append(this.mask, other.mask).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(this.ip).append(this.mask).toHashCode();
    }

}
