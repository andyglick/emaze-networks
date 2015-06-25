package net.emaze.networks.ipv6;

import java.util.Arrays;
import java.util.List;
import junit.framework.Assert;
import org.junit.Test;

public class Ipv6NetworkSplitterTest {

    @Test
    public void canSplitNetworkToDesiredNetmask() {
        final Ipv6Network net = Ipv6Network.fromCidrNotation("ffff::/16");
        final List<Ipv6Network> expected = Arrays.asList(
                Ipv6Network.fromCidrNotation("ffff:0000::/18"),
                Ipv6Network.fromCidrNotation("ffff:4000::/18"),
                Ipv6Network.fromCidrNotation("ffff:8000::/18"),
                Ipv6Network.fromCidrNotation("ffff:c000::/18")
        );

        final List<Ipv6Network> got = new Ipv6NetworkSplitter().perform(net, 18);
        Assert.assertEquals(expected, got);
    }

    @Test(expected = IllegalArgumentException.class)
    public void targetNetmaskWiderThanSourceNetmaskYieldsException() {
        final Ipv6Network net = Ipv6Network.fromCidrNotation("ffff::/16");
        new Ipv6NetworkSplitter().perform(net, 14);
    }

    @Test
    public void targetNetmaskEqualsToSourceYieldsSource() {
        final Ipv6Network net = Ipv6Network.fromCidrNotation("ffff::/16");
        final List<Ipv6Network> got = new Ipv6NetworkSplitter().perform(net, 16);
        Assert.assertEquals(true, got.size() == 1 && got.get(0).equals(net));
    }

    @Test(expected = IllegalArgumentException.class)
    public void targetNetmaskNarrowerMoreThan32BitsYieldsException() {
        final Ipv6Network net = Ipv6Network.fromCidrNotation("ffff::/16");
        new Ipv6NetworkSplitter().perform(net, 49);
    }

    @Test
    public void canSplitWith32BitsNarrowerNetmask() {
        final Ipv6Network net = Ipv6Network.fromCidrNotation("ffff::/16");
        final List<Ipv6Network> got = new Ipv6NetworkSplitter().perform(net, 32);
        Assert.assertEquals(true, got.size() == 1L << 16);
    }
}
