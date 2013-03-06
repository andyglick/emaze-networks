package net.emaze.networks;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.CompareToBuilder;
import net.emaze.dysfunctional.order.Order;

public class Cidr {

    private final Ipv4 network;
    private final Netmask netmask;

    public Cidr(Ipv4 start, Netmask netmask) {
        this.network = start.networkAddress(netmask);
        this.netmask = netmask;
    }

    public Ipv4 network() {
        return network;
    }

    public Netmask netmask() {
        return netmask;
    }

    // FIXME: this should be the next ip from network
    public Ipv4 first() {
        return network;
    }

    public Ipv4 last() {
        final int hostLength = 32 - netmask.toBits();
        final long offset = (1L << hostLength) - 1;
        return Ipv4.fromLong(network.toLong() + offset);
    }

    public boolean contains(Ipv4 ipv4) {
        //TODO Demeter?
        if (ipv4.compareTo(this.first()) == Order.LT.order()) {
            return false;
        }
        if (ipv4.compareTo(this.last()) == Order.GT.order()) {
            return false;
        }
        return true;
    }

    // FIXME: Changes from long to Ipv4 made "broken". Sometimes there is no ip after last, like
    // requested in two statements!
    public List<Cidr> exclude(Ipv4 toBeExcluded) {
        if (!this.contains(toBeExcluded)) {
            return Collections.singletonList(this);
        }
        Netmask newNetmask = netmask.narrow();
        final List<Cidr> cidrs = new ArrayList<>();

        Ipv4 lowerIp = this.first();
        // FIXME: why should I need afterLast?
        Ipv4 upperIp = new Cidr(this.first(), newNetmask).last().next();
        Cidr lowerCidr = new Cidr(lowerIp, newNetmask);
        Cidr upperCidr = new Cidr(upperIp, newNetmask);
        Ipv4 matched;
        Ipv4 unmatched;
        while (32 >= newNetmask.toBits()) {
            if (lowerCidr.contains(toBeExcluded)) {
                matched = lowerIp;
                unmatched = upperIp;
            } else if (upperCidr.contains(toBeExcluded)) {
                matched = upperIp;
                unmatched = lowerIp;
            } else {
                cidrs.add(this);
                break;
            }
            final Cidr cidr = new Cidr(unmatched, newNetmask);
            cidrs.add(cidr);
            if (newNetmask.toBits() == 32) {
                break;
            }
            newNetmask = newNetmask.narrow();
            lowerIp = matched;
            // FIXME: why should I need afterLast?
            upperIp = new Cidr(matched, newNetmask).last().next();
            lowerCidr = new Cidr(lowerIp, newNetmask);
            upperCidr = new Cidr(upperIp, newNetmask);
        }
        Collections.sort(cidrs, new FirstIpThenLastIpCidrComparator());
        return cidrs;
    }

    public static class FirstIpThenLastIpCidrComparator implements Comparator<Cidr> {

        @Override
        public int compare(Cidr lhs, Cidr rhs) {
            return new CompareToBuilder().append(lhs.first(), rhs.first()).append(lhs.last(), rhs.last()).toComparison();
        }
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
