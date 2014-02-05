package net.emaze.networks;

import java.math.BigInteger;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;
import net.emaze.dysfunctional.ranges.Range;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv4Network {

    protected final Ipv4 network;
    protected final Ipv4Mask netmask;
    private String cachedToString;

    public Ipv4Network(Ipv4 network, Ipv4Mask netmask) {
        dbc.precondition(network.mask(netmask).equals(network), "Network must not contain host information");
        this.network = network;
        this.netmask = netmask;
    }

    public static Ipv4Network fromCidrNotation(String cidr) {
        dbc.precondition(cidr != null, "cidr must be not-null");
        dbc.precondition(cidr.contains("/") && cidr.indexOf("/") == cidr.lastIndexOf("/"), "cidr must contain / separator only once");
        final String[] split = cidr.split("/");
        final Ipv4 ip = Ipv4.parse(split[0]);
        final Ipv4Mask mask = new Ipv4Mask(Integer.parseInt(split[1]));
        return new Ipv4Network(ip, mask);
    }

    public static Ipv4Network fromCidrNotation(String ip, int netmaskPopulation) {
        final Ipv4 network = Ipv4.parse(ip);
        final Ipv4Mask netmask = new Ipv4Mask(netmaskPopulation);
        return new Ipv4Network(network, netmask);
    }

    public static Ipv4Network fromCidrNotation(Ipv4 network, Ipv4Mask netmask) {
        return new Ipv4Network(network, netmask);
    }

    public static Ipv4Network byContainedIp(Ipv4 ip, Ipv4Mask netmask) {
        return new Ipv4Network(ip.mask(netmask), netmask);
    }

    public Ipv4 lastIp() {
        return new Ipv4(network.bits().or(netmask.hostBits()));
    }

    public Range<Ipv4> toRange() {
        return IpRanges.RANGESV4.closed(network, this.lastIp());
    }

    public Pair<Ipv4Network, Ipv4Network> split() {
        dbc.precondition(!netmask.isNarrowest(), "Unsplittable CIDR");
        final Ipv4Mask halfMask = netmask.narrowHosts();
        final Ipv4Network first = new Ipv4Network(network, halfMask);
        final Ipv4Network second = new Ipv4Network(lastIp().mask(halfMask), halfMask);
        return Pair.of(first, second);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Ipv4Network == false) {
            return false;
        }
        final Ipv4Network other = (Ipv4Network) obj;
        return new EqualsBuilder().append(this.network, other.network).append(this.netmask, other.netmask).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(this.network).append(this.netmask).toHashCode();
    }

    public Ipv4 firstIp() {
        return network;
    }

    public Ipv4Mask netmask() {
        return netmask;
    }

    public BigInteger size() {
        return netmask.hosts();
    }

    public Pair<Ipv4, Ipv4Mask> toCidr() {
        return Pair.of(network, netmask);
    }

    public boolean contains(Ipv4 ip) {
        if (Order.of(ip.compareTo(this.firstIp())) == Order.LT) {
            return false;
        }
        if (Order.of(ip.compareTo(this.lastIp())) == Order.GT) {
            return false;
        }
        return true;
    }

    public boolean contains(Ipv4Network other) {
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
