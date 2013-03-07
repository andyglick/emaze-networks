package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.Order;
import net.emaze.dysfunctional.tuples.Pair;

public class Cidr {

    private final Ipv4 network;
    private final Netmask netmask;

    public Cidr(Ipv4 ip, Netmask netmask) {
        // FIXME: Not sure that we should be doing this. Shouldn't we just 
        // validate network address and manipulate it in a "parse" factory?
        this.network = ip.toNetworkAddress(netmask);
        this.netmask = netmask;
    }

    public static Cidr parse(String ip, int netmaskBits) {
        final Netmask netmask = Netmask.fromBits(netmaskBits);
        final Ipv4 network = Ipv4.parse(ip).toNetworkAddress(netmask);
        return new Cidr(network, netmask);
    }
    
    public static Cidr parse(String cidrAsString) {
        dbc.precondition(cidrAsString != null, "cidr cannot be null");
        dbc.precondition(cidrAsString.contains("/"), "cidr is not in a valid format. Acceptable format is 0.0.0.0/0");
        final String[] networkAndBits = cidrAsString.split("/", 2);
        return Cidr.parse(networkAndBits[0], Integer.parseInt(networkAndBits[1]));
    }

    public Ipv4 network() {
        return network;
    }

    public Netmask netmask() {
        return netmask;
    }

    // FIXME: this should be the first usable ip, not the network address
    public Ipv4 first() {
        return network;
    }

    public Ipv4 last() {
        final int hostLength = 32 - netmask.toBits();
        final long offset = (1L << hostLength) - 1;
        return Ipv4.fromLong(network.toLong() + offset);
    }

    public Pair<Cidr, Cidr> split() {
        dbc.precondition(!netmask.isNarrowest(), "Unsplittable cidr");
        final Netmask splittedNetmask = netmask.narrow();
        final Cidr first = new Cidr(network, splittedNetmask);
        final Cidr second = new Cidr(first.last().next(), splittedNetmask);
        return Pair.of(first, second);
    }

    public boolean contains(Ipv4 ipv4) {
        if (Order.of(ipv4.compareTo(this.first())) == Order.LT) {
            return false;
        }
        if (Order.of(ipv4.compareTo(this.last())) == Order.GT) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return String.format("%s/%s", network, netmask);
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
