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

    private Network(Ip network, Mask netmask) {
        dbc.precondition(network.mask(netmask).equals(network), "Network must not contain host information");
        dbc.precondition(netmask.isNetmask(), "Mask must be a Netmask, not a hostmask");
        this.network = network;
        this.netmask = netmask;
    }

    public static Network fromCidrNotation(Ip ip, Mask netmask) {
        return new Network(ip, netmask);
    }

    public static Network fromCidrNotation(String ip, int netmaskPopulation) {
        final Ip network = Ip.parse(ip);
        final Mask netmask = Mask.net(netmaskPopulation);
        return new Network(network, netmask);
    }

    public static Network fromCidrNotation(String cidrAsString) {
        dbc.precondition(cidrAsString != null, "cidr cannot be null");
        dbc.precondition(cidrAsString.contains("/"), "cidr is not in a valid format. Acceptable format is 0.0.0.0/0");
        final String[] networkAndBits = cidrAsString.split("/", 2);
        return Network.fromCidrNotation(networkAndBits[0], Integer.parseInt(networkAndBits[1]));
    }

    public static Network byContainedIp(Ip ip, Mask netmask) {
        dbc.precondition(netmask.isNetmask(), "mask must be a netmask, not a hostmask");
        return new Network(ip.mask(netmask), netmask);
    }

    // static Network fromRange ? fromFirstAndLast ?
    public Ip firstIp() {
        return network;
    }

    public Ip lastIp() {
        final Mask hostMask = Mask.host(32 - netmask.population());
        final Ip hostPart = Ip.LAST_IP.mask(hostMask);
        return Ip.fromBits(network.toBits() | hostPart.toBits());
    }

    public Mask netmask() {
        return netmask;
    }

    public long size() {
        return netmask.hosts();
    }

    public Pair<Ip, Mask> toCidr() {
        return Pair.of(network, netmask);
    }

    public Range<Ip> toRange() {
        return new IpRanges().closed(network, lastIp());
    }

    public Pair<Network, Network> split() {
        dbc.precondition(netmask.equals(Mask.NARROWEST) == false, "Unsplittable cidr");
        final Mask splittedNetmask = netmask.narrowHosts();
        final Network first = new Network(network, splittedNetmask);
        final Network second = new Network(this.lastIp().mask(splittedNetmask), splittedNetmask);
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
    public String toString() {
        return String.format("%s/%s", network, netmask.population());
    }

    @Override
    public boolean equals(Object rhs) {
        if (rhs instanceof Network == false) {
            return false;
        }
        final Network other = (Network) rhs;
        return new EqualsBuilder().append(this.network, other.network).append(this.netmask, other.netmask).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(this.network).append(this.netmask).toHashCode();
    }
}
