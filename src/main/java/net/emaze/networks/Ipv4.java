package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.CompareToBuilder;

public class Ipv4 implements Comparable<Ipv4> {

    public static final Ipv4 LAST_IP = new Ipv4(0xFFFFFFFFL);
    public static final Ipv4 FIRST_IP = new Ipv4(0x0L);
    private final long address;

    private Ipv4(long address) {
        this.address = address;
    }

    public static Ipv4 parse(String dottedIpAddress) {
        final long address = new DottedOctetFormToLong().perform(dottedIpAddress);
        return new Ipv4(address);
    }

    public static Ipv4 fromLong(long ip) {
        return new Ipv4(ip);
    }

    public Ipv4 next() {
        dbc.state(!this.equals(LAST_IP), "There is no ip after last");
        return new Ipv4(address + 1);
    }

    public Ipv4 previous() {
        dbc.state(!this.equals(FIRST_IP), "There is no ip before first");
        return new Ipv4(address - 1);
    }

    public Ipv4 offset(long offset) {
        final long displaced = address + offset;
        dbc.precondition((displaced >= 0) && (displaced <= LAST_IP.toLong()), "Offset overflows");
        return new Ipv4(displaced);
    }

    public long toLong() {
        return address;
    }

    @Override
    public String toString() {
        return new LongToDottedOctetForm().perform(address);
    }

    public Ipv4 toNetworkAddress(Netmask netmask) {
        final long ip = address & ((((1L << netmask.toBits()) - 1) << (32L - netmask.toBits())));
        return new Ipv4(ip);
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Ipv4 == false) {
            return false;
        }
        final Ipv4 ipv4 = (Ipv4) other;
        return new EqualsBuilder().append(this.address, ipv4.address).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(address).toHashCode();
    }

    @Override
    public int compareTo(Ipv4 other) {
        return new CompareToBuilder().append(this.address, other.address).toComparison();
    }
}
