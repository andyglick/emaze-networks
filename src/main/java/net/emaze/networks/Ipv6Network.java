package net.emaze.networks;

import java.math.BigInteger;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;
import net.emaze.dysfunctional.ranges.Range;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv6Network {

    private final Ipv6 ip;
    private final Ipv6Mask mask;
    private String memoizedToString;

    private Ipv6Network(Ipv6 ip, Ipv6Mask mask) {
        dbc.precondition(ip.mask(mask).equals(ip), "Network must not contain host information");
        this.ip = ip;
        this.mask = mask;
    }

    public static Ipv6Network fromCidrNotation(Ipv6 ip, Ipv6Mask mask) {
        return new Ipv6Network(ip, mask);
    }

    public static Ipv6Network fromCidrNotation(String ip, int maskSize) {
        final Ipv6 network = Ipv6.parse(ip);
        final Ipv6Mask mask = Ipv6Mask.net(maskSize);
        return new Ipv6Network(network, mask);
    }

    /**
     * Parses an Ipv6 network from a format of type {@code x:x:x:x:x:x:x:x/nnn}.
     * Ipv6 format can be one of those described in
     * {@link Ipv6#parse(java.lang.String) Ipv6.parse}.
     */
    public static Ipv6Network fromCidrNotation(String cidrAsString) {
        dbc.precondition(cidrAsString != null, "cidr cannot be null");
        dbc.precondition(cidrAsString.contains("/") && cidrAsString.indexOf("/") == cidrAsString.lastIndexOf("/"), "cidr is not in a valid format. Acceptable format is ::/0");
        final String[] networkAndBits = cidrAsString.split("/", 2);
        return Ipv6Network.fromCidrNotation(networkAndBits[0], Integer.parseInt(networkAndBits[1]));
    }

    public static Ipv6Network byContainedIp(Ipv6 ip, Ipv6Mask mask) {
        return new Ipv6Network(ip.mask(mask), mask);
    }

    public static Ipv6Network byContainedIp(String ip, int maskSize) {
        return byContainedIp(Ipv6.parse(ip), Ipv6Mask.net(maskSize));
    }

    public Ipv6 firstIp() {
        return ip;
    }

    public Ipv6 lastIp() {
        return Ipv6.fromBits(ip.toBits().add(mask.hostMaskBits()));
    }

    public Ipv6Mask netmask() {
        return mask;
    }

    /**
     * The amount of hosts that can be contained in the network.
     */
    public BigInteger size() {
        return mask.hosts();
    }

    public Pair<Ipv6, Ipv6Mask> toCidr() {
        return Pair.of(ip, mask);
    }

    public Range<Ipv6> toRange() {
        return new Ipv6Ranges().closed(ip, lastIp());
    }

    public Pair<Ipv6Network, Ipv6Network> split() {
        dbc.precondition(mask.equals(Ipv6Mask.NARROWEST) == false, "Unsplittable cidr");
        final Ipv6Mask splittedNetmask = Ipv6Mask.net(mask.size() + 1);
        final Ipv6Network first = new Ipv6Network(ip, splittedNetmask);
        final Ipv6Network second = new Ipv6Network(this.lastIp().mask(splittedNetmask), splittedNetmask);
        return Pair.of(first, second);
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
        if (memoizedToString == null) {
            memoizedToString = String.format("%s/%s", ip, mask.size());
        }
        return memoizedToString;
    }

    @Override
    public boolean equals(Object rhs) {
        if (rhs instanceof Ipv6Network == false) {
            return false;
        }
        final Ipv6Network other = (Ipv6Network) rhs;
        return new EqualsBuilder().append(this.ip, other.ip).append(this.mask, other.mask).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(this.ip).append(this.mask).toHashCode();
    }
}
