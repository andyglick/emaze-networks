package net.emaze.networks;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.math.BigInteger;
import net.emaze.dysfunctional.ranges.Range;
import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6NetworkTest {

    private final Ipv6 ip = Ipv6.parse("2001:0DB8:0000:CD31::");
    private final Ipv6Mask mask = Ipv6Mask.net(64);
    private Ipv6Network network = Ipv6Network.fromCidrNotation(ip, mask);

    @Test(expected = IllegalArgumentException.class)
    public void parsingNullCidrThrows() {
        Ipv6Network.fromCidrNotation(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void parsingCidrWithDoubleDashYieldsException() {
        Ipv6Network.fromCidrNotation("1234:://24");
    }

    @Test(expected = IllegalArgumentException.class)
    public void parsingEmptyCidrYieldsException() {
        Ipv6Network.fromCidrNotation("");
    }

    @Test
    public void canFetchFirstIpOfANetwork() {
        final Ipv6 got = network.firstIp();
        Assert.assertEquals(ip, got);
    }

    @Test
    public void canFetchLastIpOfANetwork() {
        final Ipv6 got = network.lastIp();
        final Ipv6 expected = Ipv6.parse("2001:0DB8:0000:CD31:FFFF:FFFF:FFFF:FFFF");
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canSplitNetwork() {
        final Pair<Ipv6Network, Ipv6Network> got = network.split();
        final Pair<Ipv6Network, Ipv6Network> expected = Pair.of(
                Ipv6Network.fromCidrNotation("2001:0DB8:0000:CD31::", mask.population() + 1),
                Ipv6Network.fromCidrNotation("2001:0DB8:0000:CD31:8000::", mask.population() + 1));
        Assert.assertEquals(expected, got);
    }

    @Test(expected = IllegalArgumentException.class)
    public void canSplitNarrowestNetwork() {
        Ipv6Network.fromCidrNotation("1234::/128").split();
    }

    @Test
    public void networksContainsFirstIp() {
        Assert.assertTrue(network.contains(network.firstIp()));
    }

    @Test
    public void networksContainsLastIp() {
        Assert.assertTrue(network.contains(network.lastIp()));
    }

    @Test
    public void cidrIsDifferentFromNull() {
        Assert.assertFalse(network.equals(null));
    }

    @Test
    public void cidrIsDifferentFromOtherObjects() {
        Assert.assertFalse(network.equals(new Object()));
    }

    @Test
    public void containsYieldsTrueForIncludedNetworkAtStart() {
        final Ipv6Network container = Ipv6Network.fromCidrNotation("1234::", 16);
        final Ipv6Network contained = Ipv6Network.fromCidrNotation("1234::", 17);
        Assert.assertTrue(container.contains(contained));
    }

    @Test
    public void containsYieldsTrueForIncludedNetworkAtEnd() {
        final Ipv6Network container = Ipv6Network.fromCidrNotation("1234::", 16);
        final Ipv6Network contained = Ipv6Network.fromCidrNotation("1234:8000::", 17);
        Assert.assertTrue(container.contains(contained));
    }

    @Test
    public void containsYieldsFalseForSeparateNetwork() {
        final Ipv6Network container = Ipv6Network.fromCidrNotation("1234::", 16);
        Assert.assertFalse(container.contains(network));
    }

    @Test
    public void rangesOfNetworkYieldsExpected() {
        final Range<Ipv6> expected = IpRanges.RANGESV6.closed(Ipv6.parse("1234::"), Ipv6.parse("1234::FFFF"));
        final Range<Ipv6> got = Ipv6Network.fromCidrNotation("1234::/112").toRange();
        Assert.assertEquals(expected, got);
    }

    @Test
    public void sizeOfANetworkIsTheNumberOfContainedIps() {
        final BigInteger got = Ipv6Network.fromCidrNotation("1234::/108").size();
        Assert.assertEquals(BigInteger.valueOf(1048576), got);
    }

    @Test
    public void toCidrFormFromNetwork() {
        final Pair<Ipv6, Ipv6Mask> expected = Pair.of(Ipv6.parse("1234::"), Ipv6Mask.net(16));
        final Pair<Ipv6, Ipv6Mask> got = Ipv6Network.fromCidrNotation("1234::/16").toCidr();
        Assert.assertEquals(expected, got);
    }
    @Test
    public void canSerializeAndDeserialize() throws IOException, ClassNotFoundException {
        final Ipv6Network value = Ipv6Network.fromCidrNotation("1234::", 16);
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final ObjectOutputStream oos = new ObjectOutputStream(baos);
        oos.writeObject(value);
        final ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()));
        final Object got = ois.readObject();
        Assert.assertEquals(value, got);
    }
}
