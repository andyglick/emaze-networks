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
    private String toString;

    /**
     * Creates a new network with an Ip and a Mask.
     *
     * @param network a valid Ip
     * @param netmask a valid Netmask
     */
    private Network(Ip network, Mask netmask) {
        dbc.precondition(network.mask(netmask).equals(network), "Network must not contain host information");
        dbc.precondition(netmask.isNetmask(), "Mask must be a Netmask, not a hostmask");
        this.network = network;
        this.netmask = netmask;
    }

    /**
     * Creates a network from Ip and Netmask.
     *
     * @param ip a valid Ip
     * @param netmask a valid Netmask
     * @return a network
     */
    public static Network fromCidrNotation(Ip ip, Mask netmask) {
        return new Network(ip, netmask);
    }

    /**
     * Creates a network from a octet-form ip address and number of bits
     * reserved for network.
     *
     * @param ip an octet-form ip address
     * @param netmaskPopulation number of reserved bits for network
     * @return a network
     */
    public static Network fromCidrNotation(String ip, int netmaskPopulation) {
        final Ip network = Ip.parse(ip);
        final Mask netmask = Mask.net(netmaskPopulation);
        return new Network(network, netmask);
    }

    /**
     * Creates a network from a CIDR (x.x.x.x/y).
     *
     * @param cidrAsString
     * @return a network
     */
    public static Network fromCidrNotation(String cidrAsString) {
        dbc.precondition(cidrAsString != null, "cidr cannot be null");
        dbc.precondition(cidrAsString.contains("/"), "cidr is not in a valid format. Acceptable format is 0.0.0.0/0");
        final String[] networkAndBits = cidrAsString.split("/", 2);
        return Network.fromCidrNotation(networkAndBits[0], Integer.parseInt(networkAndBits[1]));
    }

    /**
     * Creates a network that contains the requested Ip address.
     *
     * @param ip address that network must contain
     * @param netmask a valid netmask
     * @return a network
     */
    public static Network byContainedIp(Ip ip, Mask netmask) {
        dbc.precondition(netmask.isNetmask(), "mask must be a netmask, not a hostmask");
        return new Network(ip.mask(netmask), netmask);
    }

    // static Network fromRange ? fromFirstAndLast ?
    /**
     * Returns network's first ip (network address).
     *
     * @return an Ip representing the network's network address
     */
    public Ip firstIp() {
        return network;
    }

    /**
     * Returns network's last ip (broadcast address).
     *
     * @return an Ip representing the network's broadcast address
     */
    public Ip lastIp() {
        final Mask hostMask = Mask.host(32 - netmask.population());
        final Ip hostPart = Ip.LAST_IP.mask(hostMask);
        return Ip.fromBits(network.toBits() | hostPart.toBits());
    }

    /**
     * Returns network's netmask
     *
     * @return netmask
     */
    public Mask netmask() {
        return netmask;
    }

    /**
     * Returns the number of available ip host addresses.
     *
     * @return number of host addresses
     */
    public long size() {
        return netmask.hosts();
    }

    /**
     * Returns network represented as a pair of Network address and netmask
     *
     * @return
     */
    public Pair<Ip, Mask> toCidr() {
        return Pair.of(network, netmask);
    }

    /**
     * Returns network represented as a range of ip addresses
     *
     * @return
     */
    public Range<Ip> toRange() {
        return new IpRanges().closed(network, lastIp());
    }

    /**
     * Splits network into two networks of the same size.
     *
     * @return a pair with two halves of the network
     */
    public Pair<Network, Network> split() {
        dbc.precondition(netmask.equals(Mask.NARROWEST) == false, "Unsplittable cidr");
        final Mask splittedNetmask = netmask.narrowHosts();
        final Network first = new Network(network, splittedNetmask);
        final Network second = new Network(this.lastIp().mask(splittedNetmask), splittedNetmask);
        return Pair.of(first, second);
    }

    /**
     * Checks if ip is contained inside network
     *
     * @param ip
     * @return true if ip is contained, false otherwise
     */
    public boolean contains(Ip ip) {
        if (Order.of(ip.compareTo(this.firstIp())) == Order.LT) {
            return false;
        }
        if (Order.of(ip.compareTo(this.lastIp())) == Order.GT) {
            return false;
        }
        return true;
    }

    /**
     * Checks if another network is completely contained inside network.
     *
     * @param other a network
     * @return true if other is completely contained, false otherwise
     */
    public boolean contains(Network other) {
        final Order first = Order.of(this.firstIp().compareTo(other.firstIp()));
        final Order last = Order.of(this.lastIp().compareTo(other.lastIp()));
        return (first == Order.LT || first == Order.EQ) && (last == Order.EQ || last == Order.GT);
    }

    @Override
    public String toString() {
        //Network is immutable, so we can cache the toString value
        if (toString == null) {
            toString = String.format("%s/%s", network, netmask.population());
        }
        return toString;
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
