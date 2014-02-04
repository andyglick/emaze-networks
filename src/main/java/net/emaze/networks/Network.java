package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;
import net.emaze.dysfunctional.ranges.Range;
import net.emaze.dysfunctional.tuples.Pair;

public class Network {

    private final Ip network;
    private final Mask netmask;
    private final IpPolicy policy;
    private String cachedToString;

    public Network(Ip network, Mask netmask) {
        dbc.precondition(network.version().equals(netmask.version()), "version of ip and mask must be the same");
        this.network = network;
        this.netmask = netmask;
        this.policy = network.version();
    }

    public static Network fromCidrNotation(String cidr) {
        dbc.precondition(cidr != null, "cidr must be not-null");
        dbc.precondition(cidr.contains("/") && cidr.indexOf("/") == cidr.lastIndexOf("/"), "cidr must contain / separator only once");
        final String[] split = cidr.split("/");
        final Ip ip = Ip.parse(split[0]);
        final Mask mask = new Mask(Integer.parseInt(split[1]), ip.version());
        return new Network(ip, mask);
    }

    public static Network fromCidrNotation(String ip, int netmaskPopulation) {
        final Ip network = Ip.parse(ip);
        final Mask netmask = new Mask(netmaskPopulation, network.version());
        return new Network(network, netmask);
    }

    public static Network fromCidrNotation(Ip network, Mask netmask) {
        return new Network(network, netmask);
    }

    public static Network byContainedIp(Ip ip, Mask netmask) {
        return new Network(ip.mask(netmask), netmask);
    }

    public IpPolicy version() {
        return policy;
    }

    public Ip firstIp() {
        return network;
    }

    public Ip lastIp() {
        return new Ip(network.bits().or(netmask.hostBits()), policy);
    }

    public Mask netmask() {
        return netmask;
    }

    public FixedSizeNatural size() {
        return netmask.hosts();
    }

    public Pair<Ip, Mask> toCidr() {
        return Pair.of(network, netmask);
    }

    public Range<Ip> toRange() {
        return policy.getRanges().closed(network, lastIp());
    }

    public Pair<Network, Network> split() {
        dbc.precondition(!netmask.isNarrowest(), "Unsplittable CIDR");
        final Mask halfMask = netmask.narrowHosts();
        final Network first = new Network(network, halfMask);
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
        return new EqualsBuilder().append(this.network, other.network).append(this.netmask, other.netmask).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(this.network).append(this.netmask).toHashCode();
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
