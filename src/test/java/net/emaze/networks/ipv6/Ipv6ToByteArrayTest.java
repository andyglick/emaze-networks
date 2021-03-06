package net.emaze.networks.ipv6;

import java.util.Arrays;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6ToByteArrayTest {

    private static final Ipv6ToByteArray instance = new Ipv6ToByteArray();

    @Test(expected = IllegalArgumentException.class)
    public void applyWithNullAddressThrows() {
        instance.apply(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void applyWithAddressContainDoubleDoubleColonThrows() {
        instance.apply("1234::5678::9ABC");
    }

    @Test(expected = IllegalArgumentException.class)
    public void applyWithAddressWithoutColonThrows() {
        instance.apply("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
    }

    @Test
    public void canGetBytesFromAnAddress() {
        final byte[] got = instance.apply("FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF");
        final byte[] expected = new byte[16];
        Arrays.fill(expected, (byte) 0xFF);
        Assert.assertArrayEquals(expected, got);
    }

    @Test
    public void canGetBytesFromAnAddressInIpv4Mapped() {
        final byte[] got = instance.apply("0000:0000:0000:0000:0000:FFFF:255.255.255.255");
        final byte[] expected = new byte[]{(byte) 0x0, (byte) 0x0, (byte) 0x0, (byte) 0x0, (byte) 0x0, (byte) 0x0, (byte) 0x0, (byte) 0x0, (byte) 0x0, (byte) 0x0, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF};
        Assert.assertArrayEquals(expected, got);
    }

    @Test(expected = IllegalArgumentException.class)
    public void canGetBytesFromAnAddressInMalformedIpv4MappedThrows() {
        instance.apply("0000:0000:0000:0000:0001:FFFF:255.255.255.255");
    }

    @Test(expected = IllegalArgumentException.class)
    public void passingWrongNumberOfChunksThrows() {
        instance.apply("1111:2222:3333:4444:5555:6666:7777:8888:9999");
    }

    @Test(expected = IllegalArgumentException.class)
    public void passingWrongNumberOfChunksThrowsIpv4MappedForm() {
        instance.apply("1111:2222:3333:4444:5555:6666:7777:1.2.3.4");
    }

    @Test(expected = IllegalArgumentException.class)
    public void passingWrongChunksFormatThrows() {
        instance.apply("12345:0000:0000:0000:0000:0000:0000:0000");
    }

    @Test
    public void compressedAndExtenderFormAreEquals() {
        final byte[] got = instance.apply("::");
        final byte[] expected = instance.apply("0000:0000:0000:0000:0000:0000:0000:0000");
        Assert.assertArrayEquals(expected, got);
    }

}
