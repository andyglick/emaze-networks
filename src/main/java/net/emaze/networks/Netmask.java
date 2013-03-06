package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.CompareToBuilder;

public class Netmask implements Comparable<Netmask> {

    private final int bits;

    private Netmask(int bits) {
        this.bits = bits;
    }

    public static Netmask parse(String dottedNetmask) {
        final int bits = new LongToBits().perform(new DottedOctetFormToLong().perform(dottedNetmask));;
        return new Netmask(bits);
    }

    public static Netmask fromBits(int bits) {
        return new Netmask(bits);
    }

    public int toBits() {
        return bits;
    }

    public long toLong() {
        return new BitsToLong().perform(bits);
    }

    public String toDottedNotation() {
        return new LongToDottedOctetForm().perform(new BitsToLong().perform(bits));
    }

    @Override
    public String toString() {
        return String.format("%s", bits);
    }
    
    public Netmask narrow() {
        dbc.state(bits != 32, "Cannot narrow netmask anymore");
        return new Netmask(bits + 1);
    }

    public Netmask widen() {
        dbc.state(bits != 0, "Cannot widen netmask anymore");
        return new Netmask(bits - 1);
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Netmask == false) {
            return false;
        }
        final Netmask netmask = (Netmask) other;
        return new EqualsBuilder().append(this.bits, netmask.bits).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(bits).toHashCode();
    }

    @Override
    public int compareTo(Netmask other) {
        return new CompareToBuilder().append(this.bits, other.bits).toComparison();
    }
    
    
}
