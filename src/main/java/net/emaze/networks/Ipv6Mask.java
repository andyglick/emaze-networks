package net.emaze.networks;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ShortBuffer;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;
import net.emaze.dysfunctional.order.CompareToBuilder;

public class Ipv6Mask implements Comparable<Ipv6Mask> {

    private static final BigInteger ALL_MASK = new BigInteger("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16);
    public static final int BITS = 128;
    public static final Ipv6Mask NARROWEST = new Ipv6Mask(BITS);
    public static final Ipv6Mask WIDEST = new Ipv6Mask(0);
    private final int size;

    private Ipv6Mask(int size) {
        dbc.precondition(size >= 0 && size <= Ipv6Mask.BITS, "mask size must be between 0 and %s", Ipv6Mask.BITS);
        this.size = size;
    }

    /**
     * Constructs a mask with the given size {@code x}, resulting in {@code /x}.
     *
     * @throws IllegalArgumentException if size is negative or greater than 128
     */
    public static Ipv6Mask net(int size) {
        return new Ipv6Mask(size);
    }

    /**
     * Constructs a mask counting bits from the right. This is equivalent to
     * {@code Ipv6Mask.net(Ipv6Mask.BITS - size)}.
     *
     * @throws IllegalArgumentException if size is negative or greater than 128
     */
    public static Ipv6Mask host(int size) {
        return new Ipv6Mask(BITS - size);
    }

    /**
     * Parses the netmask from the format {@code /x}, where {@code x} is the
     * network size.
     */
    public static Ipv6Mask parse(String mask) {
        dbc.precondition(mask.startsWith("/"), "mask format must be /nnn");
        return new Ipv6Mask(Integer.parseInt(mask.substring(1)));
    }

    public int size() {
        return size;
    }

    /**
     * Returns the 128 bits of the mask, with the sequences of ones on the left.
     * Because of {@code BigInteger} implementation, a byte with value 0 is
     * prepended when the mask has size greater than 0.
     * @deprecated not sure which is better, BigInteger or byte[]
     */
    public BigInteger bits() {
        return ALL_MASK.shiftRight(size).xor(ALL_MASK);
    }

    public BigInteger hostMaskBits() {
        return bits().not().and(ALL_MASK);
    }

    /**
     * Returns 8 pieces each of 16 bit, as the Ipv6 format.
     */
    public int[] pieces() {
        final int[] pieces = new int[8];
        if (size == 0) {
            return pieces;
        }
        final ShortBuffer buffer = ByteBuffer.wrap(bits().toByteArray(), 1, 16).asShortBuffer();
        for (int i = 0; i < 8; i++) {
            pieces[i] = buffer.get() & 0xFFFF;
        }
        return pieces;
    }

    public Ipv6Mask narrowHosts() {
        return new Ipv6Mask(size < Ipv6Mask.BITS ? size + 1 : size);
    }

    public Ipv6Mask widenHosts() {
        return new Ipv6Mask(size > 0 ? size - 1 : 0);
    }

    public BigInteger hosts() {
        return BigInteger.ONE.shiftLeft(Ipv6Mask.BITS - size);
    }

    @Override
    public int compareTo(Ipv6Mask other) {
        return new CompareToBuilder().append(this.size, other.size).toComparison();
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Ipv6Mask == false) {
            return false;
        }
        final Ipv6Mask mask = (Ipv6Mask) other;
        return new EqualsBuilder().append(this.size, mask.size).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(size).toHashCode();
    }

    @Override
    public String toString() {
        return "/" + size;
    }
}
