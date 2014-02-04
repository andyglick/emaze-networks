package net.emaze.networks.old;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import net.emaze.dysfunctional.Strings;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.equality.EqualsBuilder;
import net.emaze.dysfunctional.hashing.HashCodeBuilder;

public class Ipv6 implements Comparable<Ipv6> {

    public static final int PIECES = 8;
    public static final int BYTES = 16;
    public static final int BITS = 128;

    private static final BigInteger MIN_ADDRESS_IN_BITS = new BigInteger("00000000000000000000000000000000", 16);
    private static final BigInteger MAX_ADDRESS_IN_BITS = new BigInteger("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16);
    public static final Ipv6 FIRST_IP = new Ipv6(MIN_ADDRESS_IN_BITS);
    public static final Ipv6 LAST_IP = new Ipv6(MAX_ADDRESS_IN_BITS);
    private final BigInteger address;
    private String memoizedToString = null;

    private Ipv6(BigInteger bits) {
        this.address = bits;
    }

    /**
     * Parses an ip from colon-separated form.
     * <p>
     * Possible formats are:
     * <ul>
     * <li>{@code ::} undefined address</li>
     * <li>{@code ::1} loopback address</li>
     * <li>{@code ::1234:5678} ip with heading zeroes</li>
     * <li>{@code ABCD:EF01::} ip with trailing zeroes</li>
     * <li>{@code 1202:97::00FF:0} ip with an inner sequence of 2 or more pieces
     * with value 0</li>
     * <li>{@code ffff:aa:10bb:10cc:0:1012:34:1056} ip with pieces' short
     * notation</li>
     * <li>{@code ffff:00aa:10bb:10cc:0000:1012:0034:1056} ip normal form</li>
     * </ul>
     * </p>
     * <p>
     * The Ipv4 form ({@code ::FFFF:x.x.x.x}) is not supported by this method.
     * Use {@link Ipv6#ipv4Mapped(net.emaze.networks.Ipv4) Ipv6.ipv4Mapped}
     * instead.
     * </p>
     */
    public static Ipv6 parse(String ipAddress) {
        dbc.precondition(ipAddress != null, "address must be not-null");
        final String normalForm = new Ipv6FormatToNormalForm().perform(ipAddress);
        return new Ipv6(new BigInteger(normalForm.replace(":", ""), 16));
    }

    /**
     * Constructs an Ipv6 from a sequence of 128 bits.
     * @deprecated not sure which is better, BigInteger or byte[]
     */
    public static Ipv6 fromBits(BigInteger ip) {
        return new Ipv6(ip);
    }

    /**
     * Constructs an Ipv6 from 8 pieces, each of 16 bits.
     */
    public static Ipv6 fromPieces(int... pieces) {
        dbc.precondition(pieces.length == PIECES, "pieces should be %s", Ipv6.PIECES);
        final byte[] octets = new byte[pieces.length * 2 + 1];
        for (int i = 0; i < pieces.length; i++) {
            octets[2 * i + 1] = (byte) (pieces[i] >>> Byte.SIZE);
            octets[2 * i + 2] = (byte) (pieces[i] & 0xFF);
        }
        return new Ipv6(new BigInteger(octets));
    }

    /**
     * Maps an Ipv4 to Ipv6 following the RFC specification. All Ipv4 are mapped
     * as {@code ::FFFF:x.x.x.x}.
     *
     * @see <a href="http://tools.ietf.org/html/rfc4291#section-2.5.5.2">RFC
     * 4291 2.5.5.2</a>
     */
    public static Ipv6 ipv4Mapped(Ipv4 ip) {
        final int ipBits = ip.toBits();
        return fromPieces(0, 0, 0, 0, 0, 0xFFFF, ipBits >>> Short.SIZE, ipBits & 0xFFFF);
    }

    /**
     * Returns the 128 bits of this ip. If The most significant bit of the ip is
     * greater than 127, a byte with value 0 is prepended to keep the positive
     * sign.
     * @deprecated not sure which is better, BigInteger or byte[]
     */
    public BigInteger toBits() {
        return address;
    }

    /**
     * Returns the ip's 8 pieces, each of 16 bits.
     */
    public int[] pieces() {
        final int[] pieces = new int[PIECES];
        final byte[] octets = address.toByteArray();
        for (int i = 0; i < octets.length && i < BYTES; i++) {
            final int byteValue = octets[octets.length - i - 1] & 0xFF;
            final int shift = (i % 2) * Byte.SIZE;
            pieces[pieces.length - (i / 2) - 1] |= byteValue << shift;
        }
        return pieces;
    }

    public Ipv6 mask(Ipv6Mask mask) {
        dbc.precondition(mask != null, "netmask cannot be null");
        return new Ipv6(address.and(mask.bits()));
    }

    public Ipv6 next() {
        return address.equals(MAX_ADDRESS_IN_BITS) ? LAST_IP : new Ipv6(address.add(BigInteger.ONE));
    }

    public Ipv6 previous() {
        return address.equals(MIN_ADDRESS_IN_BITS) ? FIRST_IP : new Ipv6(address.subtract(BigInteger.ONE));
    }

    @Override
    public String toString() {
        if (memoizedToString == null) {
            final List<String> pieces = new ArrayList<>();
            for (int piece : pieces()) {
                pieces.add(String.format("%04x", piece));
            }
            memoizedToString = Strings.interpose(pieces, ":");
        }
        return memoizedToString;
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Ipv6 == false) {
            return false;
        }
        final Ipv6 ipv6 = (Ipv6) other;
        return new EqualsBuilder().append(this.address, ipv6.address).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(address).toHashCode();
    }

    @Override
    public int compareTo(Ipv6 other) {
        dbc.precondition(other != null, "other cannot be null");
        return address.compareTo(other.address);
    }
}
