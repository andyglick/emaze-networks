package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;
import net.emaze.dysfunctional.tuples.Pair;

public class Cidr {

    private final Ipv4 network;
    private final Mask netmask;

    public Cidr(Ipv4 network, Mask netmask) {
        dbc.precondition(network.mask(netmask).equals(network), "Network must not contain host information");
        this.network = network;
        this.netmask = netmask;
    }

    public static Cidr byContainedIp(Ipv4 ip, Mask netmask) {
        return new Cidr(ip.mask(netmask), netmask);
    }

    public static Cidr parse(String ip, int netmaskBits) {
        final Mask netmask = Mask.net(netmaskBits);
        final Ipv4 network = Ipv4.parse(ip).mask(netmask);
        return new Cidr(network, netmask);
    }

    public static Cidr parse(String cidrAsString) {
        dbc.precondition(cidrAsString != null, "cidr cannot be null");
        dbc.precondition(cidrAsString.contains("/"), "cidr is not in a valid format. Acceptable format is 0.0.0.0/0");
        final String[] networkAndBits = cidrAsString.split("/", 2);
        return Cidr.parse(networkAndBits[0], Integer.parseInt(networkAndBits[1]));
    }

    public Ipv4 firstIp() {
        return network;
    }

    public Ipv4 lastIp() {
        final int hostLength = 32 - netmask.population();
        final long displacement = (1L << hostLength) - 1;
        return network.offset(displacement);
    }

    public Mask netmask() {
        return netmask;
    }

    public Pair<Cidr, Cidr> split() {
        dbc.precondition(netmask.equals(Mask.NARROWEST) == false, "Unsplittable cidr");
        final Mask splittedNetmask = netmask.narrowHosts();
        final Cidr first = new Cidr(network, splittedNetmask);
        final Cidr second = Cidr.byContainedIp(this.lastIp(), splittedNetmask);
        return Pair.of(first, second);
    }

    public boolean contains(Ipv4 ipv4) {
        if (Order.of(ipv4.compareTo(this.firstIp())) == Order.LT) {
            return false;
        }
        if (Order.of(ipv4.compareTo(this.lastIp())) == Order.GT) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return String.format("%s/%s", network, netmask.population());
    }

    @Override
    public boolean equals(Object rhs) {
        if (rhs instanceof Cidr == false) {
            return false;
        }
        final Cidr other = (Cidr) rhs;
        return new EqualsBuilder().append(this.network, other.network).append(this.netmask, other.netmask).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(this.network).append(this.netmask).toHashCode();
    }
}
