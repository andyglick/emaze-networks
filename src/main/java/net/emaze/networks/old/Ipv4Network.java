package net.emaze.networks.old;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;
import net.emaze.dysfunctional.ranges.Range;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv4Network {

    private final Ipv4 network;
    private final Ipv4Mask netmask;
    private String toString;

    /**
     * Creates a new network with an Ipv4 and a Ipv4Mask.
     *
     * @param network a valid Ipv4
     * @param netmask a valid Netmask
     */
    private Ipv4Network(Ipv4 network, Ipv4Mask netmask) {
        dbc.precondition(network.mask(netmask).equals(network), "Network must not contain host information");
        dbc.precondition(netmask.isNetmask(), "Mask must be a Netmask, not a hostmask");
        this.network = network;
        this.netmask = netmask;
    }

    /**
     * Creates a network from Ipv4 and Netmask.
     *
     * @param ip a valid Ipv4
     * @param netmask a valid Netmask
     * @return a network
     */
    public static Ipv4Network fromCidrNotation(Ipv4 ip, Ipv4Mask netmask) {
        return new Ipv4Network(ip, netmask);
    }

    /**
     * Creates a network from a octet-form ip address and number of bits
     * reserved for network.
     *
     * @param ip an octet-form ip address
     * @param netmaskPopulation number of reserved bits for network
     * @return a network
     */
    public static Ipv4Network fromCidrNotation(String ip, int netmaskPopulation) {
        final Ipv4 network = Ipv4.parse(ip);
        final Ipv4Mask netmask = Ipv4Mask.net(netmaskPopulation);
        return new Ipv4Network(network, netmask);
    }

    /**
     * Creates a network from a CIDR (x.x.x.x/y).
     *
     * @param cidrAsString
     * @return a network
     */
    public static Ipv4Network fromCidrNotation(String cidrAsString) {
        dbc.precondition(cidrAsString != null, "cidr cannot be null");
        dbc.precondition(cidrAsString.contains("/"), "cidr is not in a valid format. Acceptable format is 0.0.0.0/0");
        final String[] networkAndBits = cidrAsString.split("/", 2);
        return Ipv4Network.fromCidrNotation(networkAndBits[0], Integer.parseInt(networkAndBits[1]));
    }

    /**
     * Creates a network that contains the requested Ipv4 address.
     *
     * @param ip address that network must contain
     * @param netmask a valid netmask
     * @return a network
     */
    public static Ipv4Network byContainedIp(Ipv4 ip, Ipv4Mask netmask) {
        dbc.precondition(netmask.isNetmask(), "mask must be a netmask, not a hostmask");
        return new Ipv4Network(ip.mask(netmask), netmask);
    }

    // static Ipv4Network fromRange ? fromFirstAndLast ?
    /**
     * Returns network's first ip (network address).
     *
     * @return an Ipv4 representing the network's network address
     */
    public Ipv4 firstIp() {
        return network;
    }

    /**
     * Returns network's last ip (broadcast address).
     *
     * @return an Ipv4 representing the network's broadcast address
     */
    public Ipv4 lastIp() {
        final Ipv4Mask hostMask = Ipv4Mask.host(32 - netmask.population());
        final Ipv4 hostPart = Ipv4.LAST_IP.mask(hostMask);
        return Ipv4.fromBits(network.toBits() | hostPart.toBits());
    }

    /**
     * Returns network's netmask
     *
     * @return netmask
     */
    public Ipv4Mask netmask() {
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
     * Returns network represented as a pair of Ipv4Network address and netmask
     *
     * @return
     */
    public Pair<Ipv4, Ipv4Mask> toCidr() {
        return Pair.of(network, netmask);
    }

    /**
     * Returns network represented as a range of ip addresses
     *
     * @return
     */
    public Range<Ipv4> toRange() {
        return new Ipv4Ranges().closed(network, lastIp());
    }

    /**
     * Splits network into two networks of the same size.
     *
     * @return a pair with two halves of the network
     */
    public Pair<Ipv4Network, Ipv4Network> split() {
        dbc.precondition(netmask.equals(Ipv4Mask.NARROWEST) == false, "Unsplittable cidr");
        final Ipv4Mask splittedNetmask = netmask.narrowHosts();
        final Ipv4Network first = new Ipv4Network(network, splittedNetmask);
        final Ipv4Network second = new Ipv4Network(this.lastIp().mask(splittedNetmask), splittedNetmask);
        return Pair.of(first, second);
    }

    /**
     * Checks if ip is contained inside network
     *
     * @param ip
     * @return true if ip is contained, false otherwise
     */
    public boolean contains(Ipv4 ip) {
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
    public boolean contains(Ipv4Network other) {
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
        if (rhs instanceof Ipv4Network == false) {
            return false;
        }
        final Ipv4Network other = (Ipv4Network) rhs;
        return new EqualsBuilder().append(this.network, other.network).append(this.netmask, other.netmask).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(this.network).append(this.netmask).toHashCode();
    }
}
