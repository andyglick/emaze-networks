package net.emaze.networks.my;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;

public class MyIpParsers {

    public static MyIp parse(String ip) {
        dbc.precondition(ip != null, "address must be not-null");
        if (ip.contains(":")) {
            return parseFromStringV6(ip);
        }
        return parseFromStringV4(ip);
    }

    public static MyIp parseFromStringV4(String dottedIpAddress) {
        dbc.precondition(dottedIpAddress != null, "address must be not-null");
        final byte[] octets = new Ipv4DottedOctetFormToByteArray().perform(dottedIpAddress);
        return new MyIp(new BigInteger(1, octets), new IpPolicy.V4());
    }

    public static MyIp parseFromStringV6(String ip) {
        dbc.precondition(ip != null, "address must be not-null");
        final BigInteger bits = new BigInteger(new Ipv6ToByteArray().perform(ip));
        return new MyIp(bits, new IpPolicy.V6());
    }

    //FIXME: If I create a BigInteger directly from bits, do I get the same result? NO
    public static MyIp parseFromBitsV4(int bits) {
        final byte firstOctet = (byte) ((bits & 0xFF000000) >>> 24);
        final byte secondOctet = (byte) ((bits & 0x00FF0000) >> 16);
        final byte thirdOctet = (byte) ((bits & 0x0000FF00) >> 8);
        final byte fourthOctet = (byte) (bits & 0x000000FF);
        final byte[] octets = new byte[]{firstOctet, secondOctet, thirdOctet, fourthOctet};
        return new MyIp(new BigInteger(1, octets), new IpPolicy.V4());
    }

    public static class Ipv4DottedOctetFormToByteArray implements Delegate<byte[], String> {

        @Override
        public byte[] perform(String dottedOctetForm) {
            dbc.precondition(dottedOctetForm != null, "dottedOctetForm cannot be null");
            final String[] split = dottedOctetForm.split("\\.");
            dbc.precondition(split.length == 4, "dottedOctetForm is not valid");
            return new byte[]{decode(split[0]), decode(split[1]), decode(split[2]), decode(split[3])};
        }

        private byte decode(String s) {
            final Integer octet = Integer.decode(s);
            dbc.precondition((octet & 0xFFFFFF00) == 0, "dotted octet form is not valid");
            return octet.byteValue();
        }

    }

    public static class Ipv6ToByteArray implements Delegate<byte[], String> {

        @Override
        public byte[] perform(String address) {
            dbc.precondition(address != null, "address cannot be null");
            dbc.precondition(address.indexOf("::") == address.lastIndexOf("::"), "IPv6 addresses can be shortened with :: only once");

            final boolean ipv4Mapped = address.contains(".");
            if (ipv4Mapped) {
                final String ipv6part = address.substring(0, address.lastIndexOf(":"));
                final String ipv4part = address.substring(address.lastIndexOf(":") + 1);
                final String[] ipv6chunks = normalize(ipv6part, 6);
                final byte[] ipv6bytes = transform(ipv6chunks, 6);
                final byte[] ipv4bytes = new Ipv4DottedOctetFormToByteArray().perform(ipv4part);
                // TODO: check ipv6 bytes for correctness: only ::FFFF is permitted.
                return concat(ipv6bytes, ipv4bytes);
            }
            final String[] chunks = normalize(address, 8);
            final byte[] bytes = transform(chunks, 8);
            return bytes;
        }

        private String[] normalize(String address, int expectedChunks) {
            final String[] chunks = address.split(":");
            final List<String> easier = Arrays.asList(chunks);
            final int needle = easier.indexOf("");
            final int add = expectedChunks - chunks.length + 1;
            easier.remove(needle);
            for (int times = 0; times != add; ++times) {
                easier.add(needle, "0000");
            }
            return easier.toArray(new String[]{});
        }

        private byte[] transform(String[] chunks, int expectedChunks) {
            dbc.precondition(chunks.length == expectedChunks, "Number of chunks mismatch");
            final byte[] out = new byte[2 * expectedChunks];
            for (int index = 0; index != chunks.length; ++index) {
                final byte[] decoded = decode(chunks[index]);
                System.arraycopy(decoded, 0, out, 2 * index, 2);
            }
            return out;
        }

        private byte[] decode(String s) {
            final Integer chunk = Integer.parseInt(s, 16); // Do not use .decode() since standard chunk form has leading zeroes.
            dbc.precondition((chunk & 0xFFFF0000) == 0, "chunk form is not valid");
            final byte firstOctet = (byte) ((chunk & 0xFF00) >> 8);
            final byte secondOctet = (byte) (chunk & 0x00FF);
            return new byte[]{firstOctet, secondOctet};
        }

        private byte[] concat(byte[] lhs, byte[] rhs) {
            final byte[] copy = new byte[lhs.length + rhs.length];
            System.arraycopy(lhs, 0, copy, 0, lhs.length);
            System.arraycopy(rhs, 0, copy, lhs.length, rhs.length);
            return copy;
        }

    }

}
