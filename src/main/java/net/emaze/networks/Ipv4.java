package net.emaze.networks;

import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;

public class Ipv4 {

    private long address;

    public static Ipv4 parse(String dottedIpAddress) {
        final Ipv4 ipv4 = new Ipv4();
        ipv4.address = new DottedOctetFormToLong().perform(dottedIpAddress);
        return ipv4;
    }

    public static Ipv4 fromLong(long ip) {
        final Ipv4 ipv4 = new Ipv4();
        ipv4.address = ip;
        return ipv4;
    }

    private Ipv4() {
    }

    public long toLong() {
        return address;
    }

    @Override
    public String toString() {
        return new LongToDottedOctetForm().perform(address);
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
}
