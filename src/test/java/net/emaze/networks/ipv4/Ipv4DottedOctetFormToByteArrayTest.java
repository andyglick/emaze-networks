package net.emaze.networks.ipv4;

import java.util.Arrays;
import org.junit.Assert;
import org.junit.Test;

public class Ipv4DottedOctetFormToByteArrayTest {

    private final static byte[] LOCALHOST_OCTETS = new byte[]{127, 0, 0, 1};

    @Test(expected = IllegalArgumentException.class)
    public void parsingANullStringYieldsException() {
        new Ipv4DottedOctetFormToByteArray().apply(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void parsingAMalformedDottedOctetFormYieldsException() {
        new Ipv4DottedOctetFormToByteArray().apply("...");
    }

    @Test(expected = IllegalArgumentException.class)
    public void parsingAnInvalidDottedOctetFormYieldsException() {
        new Ipv4DottedOctetFormToByteArray().apply("333.-1.999.256");
    }

    @Test
    public void parsingAValidDottedOctetFormStringYieldsExpected() {
        final byte[] octets = new Ipv4DottedOctetFormToByteArray().apply("127.0.0.1");
        Assert.assertTrue(Arrays.equals(LOCALHOST_OCTETS, octets));
    }

    @Test
    public void parsingAValidDottedOctetFormHexStringYieldsExpected() {
        final byte[] octets = new Ipv4DottedOctetFormToByteArray().apply("0x7F.0x00.0x00.0x01");
        Assert.assertTrue(Arrays.equals(LOCALHOST_OCTETS, octets));
    }

    @Test
    public void parsingAValidDottedOctetFormOctStringYieldsExpected() {
        final byte[] octets = new Ipv4DottedOctetFormToByteArray().apply("0177.0000.0000.0001");
        Assert.assertTrue(Arrays.equals(LOCALHOST_OCTETS, octets));
    }

    @Test
    public void parsingAValidDottedOctetFormMixedStringYieldsExpected() {
        final byte[] octets = new Ipv4DottedOctetFormToByteArray().apply("0177.0x00.0.0X01");
        Assert.assertTrue(Arrays.equals(LOCALHOST_OCTETS, octets));
    }
}
