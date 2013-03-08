package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.CompareToBuilder;

public class Ipv4 implements Comparable<Ipv4> {

    public static final long MIN_ADDRESS_SPACE = 0x00000000L;
    public static final long MAX_ADDRESS_SPACE = 0xFFFFFFFFL;
    public static final Ipv4 LAST_IP = new Ipv4(MAX_ADDRESS_SPACE);
    public static final Ipv4 FIRST_IP = new Ipv4(MIN_ADDRESS_SPACE);
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

    public long toLong() {
        return address;
    }

    @Override
    public String toString() {
        return new LongToDottedOctetForm().perform(address);
    }

    public Ipv4 mask(Netmask netmask) {
        dbc.precondition(netmask != null, "netmask cannot be null");
        final long ip = address & ((((1L << netmask.toBits()) - 1) << (32L - netmask.toBits())));
        return new Ipv4(ip);
    }

    public Ipv4 offset(long offset) {
        dbc.precondition(Math.abs(offset) <= MAX_ADDRESS_SPACE, "Offset is bigger than IP address space");
        final long displaced = address + offset;
        dbc.precondition((displaced >= MIN_ADDRESS_SPACE) && (displaced <= MAX_ADDRESS_SPACE), "Offset overflows");
        return new Ipv4(displaced);
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
        dbc.precondition(other != null, "other cannot be null"); // by contract this should have been a NPE
        return new CompareToBuilder().append(this.address, other.address).toComparison();
    }
}
