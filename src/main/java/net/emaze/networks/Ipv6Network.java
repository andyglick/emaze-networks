package net.emaze.networks;

import java.io.Serializable;
import java.math.BigInteger;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;
import net.emaze.dysfunctional.ranges.Range;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv6Network implements Serializable {

    private final Ipv6 network;
    private final Ipv6Mask netmask;
    private String cachedToString;

    public Ipv6Network(Ipv6 network, Ipv6Mask netmask) {
        this.network = network;
        this.netmask = netmask;
    }

    public static Ipv6Network fromCidrNotation(String cidr) {
        dbc.precondition(cidr != null, "cidr must be not-null");
        dbc.precondition(cidr.contains("/") && cidr.indexOf("/") == cidr.lastIndexOf("/"), "cidr must contain / separator only once");
        final String[] split = cidr.split("/");
        final Ipv6 ip = Ipv6.parse(split[0]);
        final Ipv6Mask mask = new Ipv6Mask(Integer.parseInt(split[1]));
        return new Ipv6Network(ip, mask);
    }

    public static Ipv6Network fromCidrNotation(String ip, int netmaskPopulation) {
        final Ipv6 network = Ipv6.parse(ip);
        final Ipv6Mask netmask = new Ipv6Mask(netmaskPopulation);
        return new Ipv6Network(network, netmask);
    }

    public static Ipv6Network fromCidrNotation(Ipv6 network, Ipv6Mask netmask) {
        return new Ipv6Network(network, netmask);
    }

    public static Ipv6Network byContainedIp(Ipv6 ip, Ipv6Mask netmask) {
        return new Ipv6Network(ip.mask(netmask), netmask);
    }

    public Ipv6 lastIp() {
        return new Ipv6(network.bits().or(netmask.hostBits()));
    }

    public Range<Ipv6> toRange() {
        return IpRanges.RANGESV6.closed(network, this.lastIp());
    }

    public Pair<Ipv6Network, Ipv6Network> split() {
        dbc.precondition(!netmask.isNarrowest(), "Unsplittable CIDR");
        final Ipv6Mask halfMask = netmask.narrowHosts();
        final Ipv6Network first = new Ipv6Network(network, halfMask);
        final Ipv6Network second = new Ipv6Network(lastIp().mask(halfMask), halfMask);
        return Pair.of(first, second);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Ipv6Network == false) {
            return false;
        }
        final Ipv6Network other = (Ipv6Network) obj;
        return new EqualsBuilder().append(this.network, other.network).append(this.netmask, other.netmask).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(this.network).append(this.netmask).toHashCode();
    }

    public Ipv6 firstIp() {
        return network;
    }

    public Ipv6Mask netmask() {
        return netmask;
    }

    public BigInteger size() {
        return netmask.hosts();
    }

    public Pair<Ipv6, Ipv6Mask> toCidr() {
        return Pair.of(network, netmask);
    }

    public boolean contains(Ipv6 ip) {
        if (Order.of(ip.compareTo(this.firstIp())) == Order.LT) {
            return false;
        }
        if (Order.of(ip.compareTo(this.lastIp())) == Order.GT) {
            return false;
        }
        return true;
    }

    public boolean contains(Ipv6Network other) {
        final Order first = Order.of(this.firstIp().compareTo(other.firstIp()));
        final Order last = Order.of(this.lastIp().compareTo(other.lastIp()));
        return (first == Order.LT || first == Order.EQ) && (last == Order.EQ || last == Order.GT);
    }

    @Override
    public String toString() {
        //Network is immutable, so we can cache the toString value
        if (cachedToString == null) {
            cachedToString = String.format("%s/%s", network, netmask.population());
        }
        return cachedToString;
    }

}
