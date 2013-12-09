package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;

public class Ipv4 implements Comparable<Ipv4> {

    private static final int MIN_ADDRESS_IN_BITS = 0x00000000;
    private static final int MAX_ADDRESS_IN_BITS = 0xFFFFFFFF;
    public static final Ipv4 LAST_IP = new Ipv4(MAX_ADDRESS_IN_BITS);
    public static final Ipv4 FIRST_IP = new Ipv4(MIN_ADDRESS_IN_BITS);
    private final int address;

    private Ipv4(int bits) {
        this.address = bits;
    }

    public static Ipv4 parse(String dottedIpAddress) {
        dbc.precondition(dottedIpAddress != null, "address must be not-null");
        final long address = new DottedOctetFormToLong().perform(dottedIpAddress);
        return new Ipv4((int) address);
    }

    public static Ipv4 fromBits(int ip) {
        return new Ipv4(ip);
    }
    
    public int[] octets(){
        final int[] o = new int[4];
        for(int i=0;i!=4;++i){
            o[3-i] = address >> i*8 & 0xff;
        }
        return o;
    }

    public int toBits() {
        return address;
    }

    public Ipv4 mask(Ipv4Mask mask) {
        dbc.precondition(mask != null, "netmask cannot be null");
        return new Ipv4(address & mask.bits());
    }

    public Ipv4 next() {
        return address == MAX_ADDRESS_IN_BITS ? LAST_IP : new Ipv4(address + 1);
    }

    public Ipv4 previous() {
        return address == MIN_ADDRESS_IN_BITS ? FIRST_IP : new Ipv4(address - 1);
    }

    @Override
    public String toString() {
        return new IntToDottedOctetForm().perform(address);
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
        dbc.precondition(other != null, "other cannot be null");
        final int highestDifferentBit = Integer.highestOneBit(address ^ other.address);
        if (highestDifferentBit == 0) {
            return 0;
        }
        if ((address & highestDifferentBit) != 0) {
            return 1;
        }
        return -1;
    }
}
