package net.emaze.networks.my;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.Filtering;
import net.emaze.dysfunctional.Multiplexing;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.iterations.ConstantIterator;

public class Ipv6ToByteArray implements Delegate<byte[], String> {

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
        final String[] halves = address.split("::");
        final List<String> left = halves[0].isEmpty() ? Collections.<String>emptyList() : Arrays.asList(halves[0].split(":"));
        final List<String> right = halves.length > 1 ? Arrays.asList(halves[1].split(":")) : Collections.<String>emptyList();
        final List<String> expanded = Consumers.all(Filtering.take(expectedChunks - left.size() - right.size(), new ConstantIterator<>("0")));
        return Consumers.all(Multiplexing.flatten(left, expanded, right)).toArray(new String[]{});
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
        dbc.precondition((chunk & 0xFFFF0000) == 0, "Chunk value should be between 0x0000 and 0xFFFF");
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
