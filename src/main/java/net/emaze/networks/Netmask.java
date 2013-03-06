package net.emaze.networks;

import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;

public class Netmask {

    private int bits;

    public static Netmask parse(String dottedNetmask) {
        final Netmask netmask = new Netmask();
        netmask.bits = new LongToBits().perform(new DottedOctetFormToLong().perform(dottedNetmask));;
        return netmask;
    }

    public static Netmask fromBits(int bits) {
        final Netmask netmask = new Netmask();
        netmask.bits = bits;
        return netmask;
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
}
